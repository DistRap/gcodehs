{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
import Data.GCode


import qualified Data.Map as M
import qualified System.IO as IO
import qualified System.Environment as E

import Control.Applicative
import Data.Semigroup hiding (option)

-- experimental gcode generation

g = cls G
m = cls M
s = cls StS

(<#>) a b = a <> (num b)
--(|x|) a b = a <> (sx b)

g0 = g <#> 0
g1 = g <#> 1

rapid = g0
move = g1

sx = axis' X
sy = axis' Y
sz = axis' Z

feed = param' F

xy x y = sx x <> sy y
xyz x y z = sx x <> sy y <> sz z

code x = appmod x emptyCode
gcode x = code (g <> x)
mcode x = code (m <> x)

qq = [
    m <#> 3
  , rapid <> xyz 10 5 1
  , move <> xy 15 15  <> comment "AAA" <> feed 100
  , g <#> 0
  , g <#> 92 <> sx 13
  ]

preamble = [
    g <#> 94
  , g <#> 90
  , g <#> 21
  , s <#> 12000
  , m <#> 3
  , g <#> 4 <> param' P 10
  ]

postamble = [
    m <#> 5
  , m <#> 9
  , m <#> 2
  ]


sq x y = [(0, 0), (x, 0), (x, y), (0, y), (0,0)]

circle r steps = map (\step -> rot (step * 2*pi / steps) (r/2) 0) [1..steps]
circle' rin r steps = map (\step -> rot (rin + step * 2*pi / steps) (r/2) 0) [1..steps]
closedCircle r steps = map (\step -> rot (step * 2*pi / steps) (r/2) 0) [1..(steps+1)]
closedCircle' rin r steps = map (\step -> rot (rin + step * 2*pi / steps) (r/2) 0) [1..(steps+1)]

retZ = 1
safeZ = 2
workZ = (-2)
rapidF = 250
downF = 150

up = code (rapid <> sz safeZ <> feed rapidF)
down = code (move <> sz workZ <> feed downF)

--travelCat w = intercalate tseq w
travelCat :: [[Code]] -> [Code]
travelCat (block:rest) = (travelBlock block) ++ (travelCat rest)
travelCat [] = []

travelCatDrill :: [[Code]] -> [Code]
travelCatDrill (block:rest) = (travelBlockDrill block) ++ (travelCatDrill rest)
travelCatDrill [] = []

travelBlock :: [Code] -> [Code]
travelBlock (x:rest) = [up, asRapidXY x, down, x] ++ rest
travelBlock [] = []

travelBlockDrill :: [Code] -> [Code]
travelBlockDrill (x:rest) = [up, asRapidXY x, x] ++ rest
travelBlockDrill [] = []

-- get all except Z
asRapidXY c@Code{..} =
  case getAxes [X,Y] c of
     [Just x, Just y] -> code (rapid <> xy x y)
     _ -> c
asRapidXY x = x

main :: IO ()
main = do
    let a1 = (g <> (num 3))
    let l = map code qq
    --let sqq = map (code . (rapid <> uncurry xy)) (sq 10 10)
    --let sqq = map (code . (\(x,y) -> rapid <> xy x y)) (sq 20 10)
    --let sqq = map (gcode . (\(x,y) -> xy x y <#> 0)) (sq 20 10)

    let encR = 50
    let encSteps = 100
    let encToolR = 0.6
    let encOutlineToolR = 3.175
    let off = 4
    let encRS = encR - (2*encToolR) - off

    let anchor = circle (5.3 - encOutlineToolR) 360
    let anchors = circle (30) 4

    let encInner = circle (10.4 - encOutlineToolR) 360
    let encOuter = circle (encR + off + encOutlineToolR) 3600

    let enc = circle encR encSteps
    let encS = circle' (2*pi/360 * (360 / 200) * 1.5) encRS encSteps

    let mapcode = map (gcode . (\(x,y) -> xy x y <#> 1))

    let drill = gcode . (\(x,y) -> xyz x y workZ <#> 81 <> feed 250 <> param' R retZ)

    let sqq = map drill enc
    let sqq_small = map drill encS
    --
    -- debug circles
    --let sqq = travelCat $ map (\(xc, yc) -> map (gcode . (\(x,y) -> xy (xc + x) (yc + y) <#> 0)) (closedCircle encToolR 36)) enc
    --let sqq_small = travelCat $ map (\(xc, yc) -> map (gcode . (\(x,y) -> xy (xc + x) (yc + y) <#> 0)) (closedCircle encToolR 36)) encS

    let anch = travelCat $ map (\(xc, yc) -> map (gcode . (\(x,y) -> xy (xc + x) (yc + y) <#> 1)) anchor) anchors

    let inner = mapcode encInner
    let outer = mapcode encOuter

    let pre = map code preamble
    let post = map code postamble

    --putStrLn $ ppGCodeCompact $ pre ++ (travelCatDrill [sqq, sqq_small]) ++ post
    putStrLn $ ppGCodeCompact $ pre ++ (travelCat [inner, anch, outer]) ++ post

    --putStrLn $ ppGCodeCompact $ travelCat [inner, anch, sqq_small, sqq, outer]
    --let sample = [
    --  code (g <> num 3)
    --, code (m <> num 3)
    --]
    --

    --let a = emptyCode
    --let b = appmod (g1 <> sy 1) a
    --let c = appmod (g1 <> sx 3.13 <> sy 666 <> comment "lala") a
    --let d = code (g1 <> sy 5.13)
    --let e = appmod (g0 <> sz 7.13) a
    --print $ c
    --print $ map (\x -> inGroup b x) groups
    --print $ known b

    --putStrLn $ ppGCode $ totalize [b, c, a, a, a, d, a, appmod (sz 7) a, a, e]
    --print $ totalize' [b, c, a]
    --putStrLn $ ppGCode $ fst $ totalize' [b , c, a, a, a, d, a, appmod (sz 6.5) a, a, a, e, a, a]
    --let f = appmod (sz 6.5) a
    --print f
    --putStrLn $ ppGCode $ fst $ totalize' [b, appmod (sz 6.5) a, a] -- a, a, e, a, a]
    --putStrLn $ ppGCode $ fst $ totalize' [b, f, a] -- a, a, e, a, a]
    --putStrLn $ ppGCodeCompact l

    --file <- fmap Prelude.head E.getArgs
    --f <- BS.readFile file

    --case parseOnlyGCode f of
    --    Left err -> print err
    --    Right result -> BS.putStr $ BS.pack $ ppGCode result
--  file    <- fmap Prelude.head E.getArgs
--  IO.withFile file IO.ReadMode $ \handle ->
--    runSafeT . runEffect $
--      (() <$ PA.parsed parseGCodeLine (B.hGetSome bufsize handle) )
--      >-> P.filter isRapid
--      >-> P.filter hasFeedrate
--      >-> P.map (replaceFeedrate 666)
--      >-> P.map (replaceY 3.14)
--      >-> P.map (addReplaceZ 48)
--      >-> P.map ppGCodeLine
--      >-> P.stdoutLn
