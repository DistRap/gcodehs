{- Canonnical representation

Our IR
(work in progress)
-}

module Data.GCode.Canon where

import Data.ByteString (ByteString)
import Data.GCode.Types (Axes, zeroAxes)
import Data.Map (Map)

import qualified Data.Map

data Plane = XY | YZ | ZX | UV | WU | VW
  deriving (Show, Eq, Ord)

data CutterCompenstationSide =
    CutterCompensationRight
  | CutterCompensationLeft
  | CutterCompensationOff
  deriving (Show, Eq, Ord)

type Speed = Double
type Seconds = Double

data RotationDirection = ClockWise | CounterClockWise
  deriving (Show, Eq, Ord)

data LengthUnit = Inches | MilliMeters | CentiMeters | Meters
  deriving (Show, Eq, Ord)

data HeaterType = HeatedExtruder | HeatedBed | HeatedChamber
  deriving (Show, Eq, Ord)

-- | Some heater with id or Nothing for current / default
data Heater = Heater HeaterType (Maybe Int)
  deriving (Show, Eq, Ord)

-- | Tool length compensation
data CompensationMode =
    NoCompensation -- ^ Tool length compensation is disabled
  | LengthTable    -- ^ Following moves will take into account tool offset from tool table
  | Dynamic Axes   -- ^ Apply dynamic offset
  | Add Int        -- ^ Add tool offset of the tool specified by the parameter to currently selected tool offset
  deriving (Show, Eq, Ord)

-- Like linxucnc arcs, not used, subject to change
data ArcParams = ArcParams {
    arcFirstEnd     :: Double -- ^ first second coordinates according to selected plane
  , arcSecondEnd    :: Double
  , arcFirstAxis    :: Double
  , arcSecondAxis   :: Double
  , arcRotation     :: Int
  , arcAxisEndPoint :: Double
  , arcA :: Double
  , arcB :: Double
  , arcC :: Double
  , arcU :: Double
  , arcV :: Double
  , arcW :: Double
  } deriving (Eq, Show, Ord)

data Canon =
    StraightTraverse Axes -- ^ Rapid motion to end position specified by Axes
  | StraightFeed     Axes -- ^ Machining motion
  | StraightProbe    Axes -- ^ Straight probe towards workpeice
  | SetCoords        Axes -- ^ Set coordinates to provided values without motion
  | ArcFeed ArcParams     -- ^ Movement along arc
  | ProgramEnd            -- ^ End of the program
  | SetFeedRate Speed     -- ^ Set feed rate for machining moves
  | SetTraverseRate Speed -- ^ Set feed rate for travel moves
  | PlaneSelect Plane     -- ^ Set plane
  | PauseSeconds Double   -- ^ Do nothing for specified number of seconds
  | SpindleStart {
      spindleDirection    :: RotationDirection -- ^ Rotate spindle according to `RotationDirection`
    , spindleWaitForSpeed :: Bool              -- ^ Wait for spindle to reach desired RPM
    }
  | SpindleStop          -- ^ Stop spindle
  | SpindleSpeed Speed   -- ^ Set spindle RPM
  | CoolantMist          -- ^ Enable mist coolant
  | CoolantFlood         -- ^ Enable flood coolant
  | CoolantStop          -- ^ Stop all coolant flows
  -- Tools
  | ToolSelect Int       -- ^ Select tool by its index
  | ToolChange           -- ^ Perform tool change
  | ToolLengthCompensation CompensationMode -- ^ Enable tool length compensation
  -- Printer
  | FanOn                -- ^ Enable fan
  | FanOff               -- ^ Disable fan
  | SetTemperature Heater Double      -- ^ Set temperature of the specific heater
  | SetTemperatureWait Heater Double  -- ^ Set temperature and wait for it to be reached
  | CancelWaitTemperature             -- ^ Cancel all temperature waits
  | LevelBed                          -- ^ Perform automated bed leveling
  -- Misc
  | Yield -- or Await? or? for blocking cmd
  | DisableMotors Axes                -- ^ Disable power to motors
  | DisplayMessage ByteString         -- ^ Display a message, typically on LCD
  | Comment ByteString                -- ^ Just a comment
  deriving (Show, Eq, Ord)

-- | State of the Canon interpreter
data CanonState = CanonState {
    canonPosition     :: Axes        -- ^ Position
  , canonTraverseRate :: Speed       -- ^ Speed for travel moves
  , canonFeedRate     :: Speed       -- ^ Speed for machining moves
  , canonTraversing   :: Bool        -- ^ True if we are performing a travel move
  , canonPlane        :: Plane       -- ^ Selected plane
  , canonLastComment  :: ByteString  -- ^ Last comment we saw
  -- XXX: needs indexes and power
  , canonFans         :: Bool        --
  , canonHeaters      :: Map Heater Double
-- XXX: needs indexes and power??
, canonCooling      :: Bool        --
  , canonFinished     :: Bool        -- True if we are done
  } deriving (Show, Eq, Ord)

data WaitingFor = WaitingHeaters | WaitingSpindle -- | WaitingEverything
  deriving (Show, Eq, Ord)

--cancelWaits :: WaitingFor -> CanonState -> CanonState
--cancelWaits for s = case (canonWaiting s) of
--  (Just w) | w == for -> s { canonWaiting = Nothing }

-- | Initial state of the Canon interpreter
initCanonState :: CanonState
initCanonState = CanonState {
    canonPosition     = zeroAxes
  , canonTraversing   = True
  , canonTraverseRate = 0
  , canonFeedRate     = 0
  , canonPlane        = XY
  , canonLastComment  = mempty
  , canonFans         = False
  , canonHeaters      = mempty
  , canonCooling      = False
  , canonFinished     = False
  }

-- | Step Canon interpreter, returning new state
stepCanon :: CanonState -> Canon -> CanonState
stepCanon s (StraightTraverse a) = s {
    canonPosition = Data.Map.union a (canonPosition s)
  , canonTraversing = True
  }
stepCanon s (StraightFeed a) = s {
    canonPosition = Data.Map.union a (canonPosition s)
  , canonTraversing = False
  }
stepCanon s (SetFeedRate r)     = s { canonFeedRate = r }
stepCanon s (SetTraverseRate r) = s { canonTraverseRate = r }
stepCanon s (SetCoords a)       = s { canonPosition = Data.Map.union a (canonPosition s) }
stepCanon s (PlaneSelect p)     = s { canonPlane = p }
stepCanon s (Comment c)         = s { canonLastComment = c }
stepCanon s (PauseSeconds x)    = s
stepCanon s ProgramEnd    = s { canonFinished = True }

stepCanon s (SetTemperature heater value) = s { canonHeaters = Data.Map.insert heater value (canonHeaters s) }
--stepCanon s CancelWaitTemperature = cancelWaits WaitingHeaters s
stepCanon s _ = s
-- TODO
--stepCanon _s e@(StraightProbe _) = error $ "Don't know how to step " ++ show e
--stepCanon _s e@(ArcFeed _) = error $ "Don't know how to step " ++ show e
--stepCanon _s e@(SpindleStart _ _) = error $ "Don't know how to step " ++ show e
--stepCanon _s e@(SpindleStop) = error $ "Don't know how to step " ++ show e
--stepCanon _s e@(SpindleSpeed _) = error $ "Don't know how to step " ++ show e
--stepCanon _s e@(ToolChange) = error $ "Don't know how to step " ++ show e
--stepCanon _s e@(ToolSelect _) = error $ "Don't know how to step " ++ show e
--stepCanon _s e@(ToolLengthCompensation _) = error $ "Don't know how to step " ++ show e
--stepCanon _s e@(CoolantMist) = error $ "Don't know how to step " ++ show e
--stepCanon _s e@(CoolantFlood) = error $ "Don't know how to step " ++ show e
--stepCanon _s e@(CoolantStop) = error $ "Don't know how to step " ++ show e
--stepCanon s e = error $ "Don't know how to step " ++ show e

-- | Fully eval list of `Canon` commands.
--
-- Slow, only useful for testing, use `Data.GCode.Pipes` variant instead
evalCanon :: (CanonState -> CanonState -> Canon -> [a])
          -> [Canon]
          -> [a]
evalCanon f cs = go initCanonState cs
  where
    go _ [] = []
    go st (c:rest) =
      let
        newSt = stepCanon st c
      in (f st newSt c) ++ (go newSt rest)
