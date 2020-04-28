{- Canonnical representation

Our IR
(work in progress)
-}

module Data.GCode.Canon where

import Data.GCode.Types (Axes)

data Plane = XY | YZ | XZ | UV | VW | UW
  deriving (Show, Eq, Ord)

data CutterSide = Right | Left | Off
  deriving (Show, Eq, Ord)

type Speed = Double
type Seconds = Double

data Rotation = Clockwise | CounterClockwise
  deriving (Show, Eq, Ord)

data LengthUnit = Inches | MilliMeters | CentiMeters | Meters
  deriving (Show, Eq, Ord)

data LineNumbered a = Line Integer a
  deriving (Show, Eq, Ord)

data Canon =
    StraightTraverse Axes -- rapid motion to end position specified by Axes
  | StraightFeed Axes     -- machining motion
  | StraightProbe Axes
  -- XXX: maybe embed plane into arc
  | ArcFeed {             -- first second coordinates according to selected plane
      arcFirstEnd :: Double
    , arcSecondEnd :: Double
    , arcFirstAxis :: Double
    , arcSecondAxis :: Double
    , arcRotation :: Int
    , arcAxisEndPoint :: Double
    , arcA :: Double
    , arcB :: Double
    , arcC :: Double
    , arcU :: Double
    , arcV :: Double
    , arcW :: Double
    }
--  | NURBSFeed NURBPoints
  | Stop
  | Dwell Seconds
  | SetFeedRate Speed
  | SetTraverseRate Speed
  | SetPlane Plane
  | SetLengthUnits LengthUnit
  | PauseSeconds Double
  | Spindle {
        spindleTurning :: Bool
      , spindleRotation :: Rotation
      , spindleSpeed :: Speed
      , spindleWaitForSpeed :: Bool
      }
  | SpindleSpeed Speed
  deriving (Show, Eq, Ord)

data CanonState = CanonState {
    canonPosition :: Axes
  , canonTraverseRate :: Speed
  , canonFeedRate :: Speed
  , canonPlane :: Plane
  } deriving (Show, Eq, Ord)

stepCanon :: CanonState -> Canon -> CanonState
stepCanon cs c = undefined

evalCanon :: (Canon -> a) -> [Canon] -> [a]
evalCanon = undefined

--toLines :: [Canon] -> [(Bool, (Double, Double, Double), (Double, Double, Double) )]
