module Data.GCode.RS274.Types where

import Data.GCode.Types

 -- G-code command definition
data GCodeDef = GCodeDef
  { defCls :: Maybe Class
  , defNum :: Maybe Int
  , defSub :: Maybe Int
  , defGroup :: RS274Group
  , defName :: RS274Name
  , defHelp :: String
  } deriving (Show, Eq, Ord)

defGCD = GCodeDef {
    defCls = Nothing
  , defNum = Nothing
  , defSub = Nothing
  , defGroup = Unknown
  , defName = Unnamed
  , defHelp = ""
  }

-- utils for creating GCodeDefs
defG = defGCD { defCls = Just G }
defM = defGCD { defCls = Just M }
g x n = defG { defNum = Just x , defName = n }
m x n = defM { defNum = Just x , defName = n }
gsub x s n = (g x n) { defSub = Just s }
msub x s n = (m x n) { defSub = Just s }

(&) = flip ($)
help txt x = x {defHelp = txt }

-- turn `GCodeDef` into `Code`
toCode :: GCodeDef -> Code
toCode x = emptyCode { codeCls = defCls x
                     , codeNum = defNum x
                     , codeSub = defSub x
                     }

-- set defGroup for each list member
makeGroup :: RS274Group -> [GCodeDef] -> [GCodeDef]
makeGroup group defs = map (\x -> x { defGroup = group }) defs

data RS274Name =
    Unnamed
  | Rapid
  | Move
  | ArcCW
  | ArcCCW
  | Dwell
  | CubicSpline
  | QuadSpline
  | NURBS
  | XYZPlane
  | XZYPlane
  | YZXPlane
  | Inches
  | Millimeters
  | SpindleSync
  | RigidTap
  | Probe
  | DrillingCycleCB
  | ThreadingCycle
  | DrillingCycleCancel
  | DrillingCycle
  | DrillingCycleDwell
  | DrillingCyclePeck
  | BoringCycle
  | BoringCycleDwell
  | Absolute
  | Relative
  | ArcAbsolute
  | ArcRelative
  | LatheDiameter
  | LatheRadius
  | InverseTime
  | UnitsPerMinute
  | UnitsPerRevolution
  | SpindleOrient
  | SpindleStop
  | SpindleCW
  | SpindleCCW
  | SpindleModeConstantSurfaceSpeed
  | SpindleModeRPM
  | CoolantMist
  | CoolantFlood
  | CoolantStop
  | ToolLength
  | ToolLengthDynamic
  | ToolLengthAdd
  | ToolLengthCancel
  | Pause
  | OptionalPause
  | ProgramEnd
  | PalletChange
  | PalletChangePause
  | CutterCompensationOff
  | CutterCompensationLeft
  | CutterCompensationDynamicLeft
  | CutterCompensationRight
  | CutterCompensationDynamicRight
  | ToolChange
  | SetCurrentTool
  | SetToolTable -- XXX this is composed from like five different commands with L parameter
  | StoredPositionMove
  | StoredPositionSet
  | ToolChangePositionMove
  | ToolChangePositionSet
  | MoveInMachineCoordinates
  | CoordinateSystemOffset
  | ResetOffsetsParams
  | ResetOffsets
  | RestoreOffsets
  | OverridesEnable
  | OverridesDisable
  | FeedRateOverride
  | SpindleSpeedOverride
  | AdaptiveFeedControl
  | FeedStopControl
 deriving (Eq, Ord, Show)

data RS274Group =
    Motion
  | Cycles
  | Distance
  | FeedRateMode
  | SpindleControl
  | CoolantControl
  | Stopping
  | Units
  | Plane
  | ToolLengthOffset
  | CutterRadius
  | OtherModal
  | NonModal
  | Unknown
 deriving (Eq, Ord, Show)


-- G-Codes

groupMotion = makeGroup Motion [
    g 0 Rapid
      & help "Rapid move"
  , g 1 Move
      & help "Linear move"
  , g 2 ArcCW
      & help "Clock-wise arc"
  , g 3 ArcCCW
      & help "Counter clock-wise arc"
  , g 4 Dwell
      & help "Do nothing for specified time"
  , g 5 CubicSpline
      & help "Cubic B-spline move"
  , gsub 5 1 QuadSpline
      & help "Quadratic B-spline move"
  , gsub 5 2 NURBS
      & help "NURBS curve move"

  , g 17 XYZPlane
      & help "Select XYZ plane (default)"
  , g 18 XZYPlane
      & help "Select XZY plane"
  , g 19 YZXPlane
      & help "Select YZX plane"

  , g 20 Inches
      & help "Set units to inches"
  , g 21 Millimeters
      & help "Set units to millimeters"

  , g 33 SpindleSync
      & help "Perform spindle synchronized motion"
  , gsub 33 1 RigidTap
      & help "Rigid Tapping"
  , g 38 Probe
      & help "Straight probe"
  ]

groupCutterRadius = makeGroup CutterRadius [
    g 40      CutterCompensationOff
  , g 41      CutterCompensationLeft
  , gsub 41 1 CutterCompensationDynamicLeft
  , g 42      CutterCompensationRight
  , gsub 42 1 CutterCompensationDynamicRight
  ]

groupToolLengthOffset = makeGroup ToolLengthOffset [
    g 43      ToolLength
      & help "Enables tool length compensation"
  , gsub 43 1 ToolLengthDynamic
  , gsub 43 2 ToolLengthAdd
      & help "Apply additional tool length offset"
  , g 49      ToolLengthCancel
      & help "Cancel tool length compensation"
  ]

groupCycles = makeGroup Cycles [
    g 73 DrillingCycleCB
  , g 76 ThreadingCycle
  , g 80 DrillingCycleCancel
  , g 81 DrillingCycle
  , g 82 DrillingCycleDwell
  , g 83 DrillingCyclePeck
  , g 85 BoringCycle
  , g 89 BoringCycleDwell
  ]

groupDistance = makeGroup Distance [
    g 90 Absolute
      & help "Absolute distance mode"
  , g 91 Relative
      & help "Incremental distance mode"
  , gsub 90 1 ArcAbsolute
      & help "Absolute distance mode for I, J & K offsets"
  , gsub 91 1 ArcRelative
      & help "Incremental distance mode for I, J & K offsets"
  , g 7 LatheDiameter
  , g 8 LatheRadius
  ]

groupFeedRateMode = makeGroup FeedRateMode [
    g 93 InverseTime
      & help "Iverse time feed rate mode, move should be completed in 1/F minutes"
  , g 94 UnitsPerMinute
      & help "Feed rates in units per minute"
  , g 95 UnitsPerRevolution
      & help "Feed rates in units per revolution"
  ]


-- mixed M/G

groupSpindleControl = makeGroup SpindleControl [
    m 3  SpindleCW
      & help "Start the spindle clockwise at the S speed"
  , m 4  SpindleCCW
      & help "Start the spindle counterclockwise at the S speed"
  , m 5  SpindleStop
      & help "Stop spindle"
  , m 19 SpindleOrient
      & help "Orient spindle"
  , g 96 SpindleModeConstantSurfaceSpeed
  , g 97 SpindleModeRPM
  ]

-- M-Codes

groupStopping = makeGroup Stopping [
    m 0 Pause
      & help "Pause a running program temporarily"
  , m 1 OptionalPause
      & help "Pause a running program temporarily if the optional stop switch is on"
  , m 2 ProgramEnd
      & help "End the program"
  , m 30 PalletChange
      & help "Exchange pallet shuttles and end the program"
  , m 60 PalletChangePause
      & help "Exchange pallet shuttles and then pause a running program temporarily"
  ]

groupCoolantControl = makeGroup CoolantControl [
    m 7 CoolantMist
      & help "Turn mist coolant on"
  , m 8 CoolantFlood
      & help "Turn flood coolant on"
  , m 9 CoolantStop
      & help "Stop both coolants (M7 & M8)"
  ]

-- non-modal codes
groupNonModal = makeGroup NonModal [
    m 6 ToolChange
      & help "Stop machine and prompt for tool change"
  , m 61 SetCurrentTool
      & help "Change current tool number without tool-change (in MDI/Manual mode only)"
  , g 10 SetToolTable -- XXX this is composed from like five different commands with L parameter
  , g 28 StoredPositionMove
      & help "Make a rapid move to position stored with G28.1"
  , gsub 28 1 StoredPositionSet
      & help "Store current absolute position"
  , g 30 ToolChangePositionMove
      & help "Make a rapid move to position stored with G30.1"
  , gsub 30 1 ToolChangePositionSet
      & help "Store current absolute position as tool change position"
  , g 53 MoveInMachineCoordinates
      & help "Move in the machine coordinate system"
  , g 92 CoordinateSystemOffset
      & help "Make the current point have the coordinates you want (without motion)"
  , gsub 92 1 ResetOffsetsParams
      & help "Turn off G92 offsets and reset parameters 5211 - 5219 to zero"
  , gsub 92 2 ResetOffsets
      & help "Turn off G92 offsets but keep parameters 5211 - 5219 available"
  , gsub 92 3 RestoreOffsets
      & help "Set the G92 offsets to the values saved in parameters 5211 - 5219"
  ]

groupOtherModal = makeGroup OtherModal [
    defGCD { defCls = Just StF }
      & help "Set feed rate"
  , defGCD { defCls = Just StS }
      & help "Set spindle speed"
  , defGCD { defCls = Just T }
      & help "Select tool"
  , m 48 OverridesEnable
      & help "Enable the spindle speed and feed rate override controls"
  , m 49 OverridesDisable
      & help "Disable the spindle speed and feed rate override controls"
  , m 50 FeedRateOverride
      & help "Feed rate override control"
  , m 51 SpindleSpeedOverride
      & help "Spindle speed override control"
  , m 52  AdaptiveFeedControl
      & help "Adaptive feed control"
  , m 53 FeedStopControl
      & help "Feed stop control"
  ]

allGroups = [
    (Motion           , groupMotion)
  , (ToolLengthOffset , groupToolLengthOffset)
  , (Cycles           , groupCycles)
  , (Distance         , groupDistance)
  , (FeedRateMode     , groupFeedRateMode)
  , (SpindleControl   , groupSpindleControl)
  , (Stopping         , groupStopping)
  , (CoolantControl   , groupCoolantControl)
  , (CutterRadius     , groupCutterRadius)
  , (OtherModal       , groupOtherModal)
  , (NonModal         , groupNonModal)
  ]

groupNames = map fst allGroups

allCodes = concatMap snd allGroups
