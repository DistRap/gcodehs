{-| GCode generation

GCode generation functions & shortcuts

-}
module Data.GCode.Generate where

import Data.GCode.Types
import Data.GCode.RS274
import Data.GCode.Utils
import Data.GCode.Pretty
import Data.Semigroup hiding (option)

-- |Turn `CodeMod`ifier(s) into `Code`
code :: CodeMod -> Code
code x = appmod x emptyCode

-- |Map CodeMod generating function over list
mapCode :: (a -> CodeMod) -> [a] -> [Code]
mapCode f = map (code . f)

-- |Turn a list of `CodeMod`ifier(s) into `GCode`
toGCode :: [CodeMod] -> GCode
toGCode = map code

-- |Set Code class to G
g :: CodeMod
g = cls G

-- |Set Code class to M
m :: CodeMod
m = cls M

-- |Set Code class to S
s :: CodeMod
s = cls StS

-- |Set GCode number
(<#>) :: CodeMod -> Int -> CodeMod
(<#>) a b = a <> (num b)

-- |Set GCode feedrate (F parameter)
feed :: Double -> CodeMod
feed = param F

-- |Set `x` axis target
x :: Double -> CodeMod
x = axis X

-- |Set `y` axis target
y :: Double -> CodeMod
y = axis Y

-- |Set `z` axis target
z :: Double -> CodeMod
z = axis Z

-- |Set `x`, `y` coordinates for this Code
xy :: Double -> Double -> CodeMod
xy xVal yVal = x xVal <> y yVal

-- |Set `x`, `y` and `z` coordinates
xyz :: Double -> Double -> Double -> CodeMod
xyz xVal yVal zVal = x xVal <> y yVal <> z zVal

-- |Set G0 and `x`, `y` coordinates
movexy x y = move <> xy x y

-- |Generate points on a rectangle
rectangle :: (Num a, Num b) => a -> b -> [(a, b)]
rectangle x y = [(0, 0), (x, 0), (x, y), (0, y), (0,0)]

-- |Rotate X/Y coordinates by angle `by`
rot by x y = (x * (cos by) - y * (sin by), y * (cos by) + x * (sin by))

-- |Generate a list of points laying on a circle with radius `r`, divides circle in `steps` number of points
circle :: (Floating b, Enum b) => b -> b -> [(b, b)]
circle r steps = map (\step -> rot (step * 2*pi / steps) (r/2) 0) [1..steps]

-- |As `circle` with rotated by `rin`
circle' :: (Floating b, Enum b) => b -> b -> b -> [(b, b)]
circle' rin r steps = map (\step -> rot (rin + step * 2*pi / steps) (r/2) 0) [1..steps]

-- |As `circle` but origin is the same as end point
closedCircle r steps = map (\step -> rot (step * 2*pi / steps) (r/2) 0) [1..(steps+1)]

-- |Join list of GCodes with travel moves inbetween
travelCat :: Code -> Code -> [GCode] -> [Code]
travelCat up down (block:rest) = (travel up down block) ++ (travelCat up down rest)
travelCat _ _ [] = []

-- |Join list of drilling GCodes with travel moves inbetween
travelCatDrill :: Code -> [GCode] -> [Code]
travelCatDrill up (block:rest) = (travelDrills up block) ++ (travelCatDrill up rest)
travelCatDrill _ [] = []

-- |Prepend codes with tool up command, rapid move to block start and tool down command
--
-- Prepends `up` GCode representing tool moving up before
-- rapid move followed by `down` command to move tool down again.
travel :: Code -> Code -> GCode -> GCode
travel up down (x:rest) = [up, asRapidXY x, down, x] ++ rest
travel _ _ [] = []

-- |Prepend drilling codes with tool up command and rapid moves
--
-- Prepends `up` GCode representing tool moving up before
-- rapid move to start of this block
travelDrills :: Code -> GCode -> GCode
travelDrills up block = travel up emptyCode block

-- |Take X and Y coordinates of this code
-- and turn it into rapid move
asRapidXY c@Code{} =
  case getAxes [X,Y] c of
     [Just x, Just y] -> code (rapid <> xy x y)
     _ -> c
asRapidXY x = x
