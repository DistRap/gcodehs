{-| GCode generation

GCode generation functions & shortcuts

-}
module Data.GCode.Generate where

import Data.GCode.Types
import Data.GCode.RS274
import Data.GCode.Utils
import Data.GCode.Pretty
import Data.Semigroup hiding (option)

-- |Generate G Code
g :: Code
g = cls G emptyCode

-- |Generate M Code
m :: Code
m = cls M emptyCode

-- |Generate S (set spindle feedrate) Code
s :: Code
s = emptyCode & cls SStandalone

-- |Set GCode number
(<#>) :: Code -> Int -> Code
(<#>) a n = num n a

-- |Set GCode feedrate (F parameter)
feed :: Double -> Code -> Code
feed = param F

-- |Set `x` axis target
x :: Double -> Code -> Code
x = axis X

-- |Set `y` axis target
y :: Double -> Code -> Code
y = axis Y

-- |Set `z` axis target
z :: Double -> Code -> Code
z = axis Z

-- |Set `x`, `y` coordinates for this Code
xy :: Double -> Double -> Code -> Code
xy xVal yVal = x xVal . y yVal

-- |Set `x`, `y` and `z` coordinates
xyz :: Double -> Double -> Double -> Code -> Code
xyz xVal yVal zVal = x xVal . y yVal . z zVal

-- |Set G0 and `x`, `y` coordinates
movexy x y = move & xy x y

-- |Set `i`, `j` parameters for this Code
ij :: Double -> Double -> Code -> Code
ij iVal jVal = param I iVal . param J jVal

arc = arcCW

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
     [Just x, Just y] -> rapid & xy x y
     _ -> c
asRapidXY x = x
