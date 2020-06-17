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

defGCD :: GCodeDef
defGCD = GCodeDef {
    defCls = Nothing
  , defNum = Nothing
  , defSub = Nothing
  , defGroup = Unknown
  , defName = Unnamed
  , defHelp = ""
  }

-- utils for creating GCodeDefs
defG :: GCodeDef
defG = defGCD { defCls = Just G }

defM :: GCodeDef
defM = defGCD { defCls = Just M }

g :: Int -> RS274Name -> GCodeDef
g x n = defG { defNum = Just x , defName = n }

m :: Int -> RS274Name -> GCodeDef
m x n = defM { defNum = Just x , defName = n }

gsub :: Int -> Int -> RS274Name -> GCodeDef
gsub x s n = (g x n) { defSub = Just s }

msub :: Int -> Int -> RS274Name -> GCodeDef
msub x s n = (m x n) { defSub = Just s }

-- | Add help text to `GCodeDef`
help :: String -> GCodeDef -> GCodeDef
help txt x = x {defHelp = txt }

-- | Turn `GCodeDef` into `Code`
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
  | XYPlane
  | ZXPlane
  | YZPlane
  | UVPlane
  | WUPlane
  | VWPlane
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
  -- 3D printer specific
  | ExtruderAbsolute
  | ExtruderRelative
  | SetExtruderTemperature
  | GetExtruderTemperature
  | SetExtruderTemperatureAndWait
  | SetBedTemperature
  | SetBedTemperatureAndWait
  | SetChamberTemperature
  | SetChamberTemperatureAndWait
  | CancelWaitTemperature
  | FanOn
  | FanOff
  | GetCurrentPosition
  | DisplayMessage
  | DisableActuators
  | AutoBedLevel
 deriving (Eq, Ord, Show)

data RS274Group =
    Motion
  | Cycles
  | Distance
  | ArcDistance
  | FeedRateMode
  | SpindleControl
  | CoolantControl
  | Stopping
  | Units
  | Plane
  | ToolLengthOffset
  | CutterRadius
  | LatheDiameterMode
  | OtherModal
  | NonModal
  | Unknown
  -- 3D printer specific
  | Extruder
  | Heating
  | Cooling
  | PrinterMisc
 deriving (Eq, Ord, Show)


-- G-Codes

groupMotion :: [GCodeDef]
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
  , g 33 SpindleSync
      & help "Perform spindle synchronized motion"
  , gsub 33 1 RigidTap
      & help "Rigid Tapping"
  , g 38 Probe
      & help "Straight probe"
  ]

groupPlane :: [GCodeDef]
groupPlane = makeGroup Plane [
    g 17 XYPlane
      & help "Select XY plane (default)"
  , g 18 ZXPlane
      & help "Select ZX plane"
  , g 19 YZPlane
      & help "Select YZ plane"
  , gsub 17 1 UVPlane
      & help "Select UV plane"
  , gsub 18 1 WUPlane
      & help "Select WU plane"
  , gsub 19 1 VWPlane
      & help "Select VW plane"
  ]

groupUnits :: [GCodeDef]
groupUnits = makeGroup Units [
    g 20 Inches
      & help "Set units to inches"
  , g 21 Millimeters
      & help "Set units to millimeters"
  ]

groupCutterRadius :: [GCodeDef]
groupCutterRadius = makeGroup CutterRadius [
    g 40      CutterCompensationOff
  , g 41      CutterCompensationLeft
  , gsub 41 1 CutterCompensationDynamicLeft
  , g 42      CutterCompensationRight
  , gsub 42 1 CutterCompensationDynamicRight
  ]

groupToolLengthOffset :: [GCodeDef]
groupToolLengthOffset = makeGroup ToolLengthOffset [
    g 43      ToolLength
      & help "Enables tool length compensation"
  , gsub 43 1 ToolLengthDynamic
  , gsub 43 2 ToolLengthAdd
      & help "Apply additional tool length offset"
  , g 49      ToolLengthCancel
      & help "Cancel tool length compensation"
  ]

groupCycles :: [GCodeDef]
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

groupDistance :: [GCodeDef]
groupDistance = makeGroup Distance [
    g 90 Absolute
      & help "Absolute distance mode"
  , g 91 Relative
      & help "Incremental distance mode"
  ]

groupArcDistance :: [GCodeDef]
groupArcDistance = makeGroup ArcDistance [
    gsub 90 1 ArcAbsolute
      & help "Absolute distance mode for I, J & K offsets"
  , gsub 91 1 ArcRelative
      & help "Incremental distance mode for I, J & K offsets"
  ]

groupLatheDiameterMode :: [GCodeDef]
groupLatheDiameterMode = makeGroup LatheDiameterMode [
    g 7 LatheDiameter
  , g 8 LatheRadius
  ]

groupFeedRateMode :: [GCodeDef]
groupFeedRateMode = makeGroup FeedRateMode [
    g 93 InverseTime
      & help "Iverse time feed rate mode, move should be completed in 1/F minutes"
  , g 94 UnitsPerMinute
      & help "Feed rates in units per minute"
  , g 95 UnitsPerRevolution
      & help "Feed rates in units per revolution"
  ]


-- mixed M/G

groupSpindleControl :: [GCodeDef]
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

groupStopping :: [GCodeDef]
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

groupCoolantControl :: [GCodeDef]
groupCoolantControl = makeGroup CoolantControl [
    m 7 CoolantMist
      & help "Turn mist coolant on"
  , m 8 CoolantFlood
      & help "Turn flood coolant on"
  , m 9 CoolantStop
      & help "Stop both coolants (M7 & M8)"
  ]

-- non-modal codes
groupNonModal :: [GCodeDef]
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

groupOtherModal :: [GCodeDef]
groupOtherModal = makeGroup OtherModal [
    defGCD { defCls = Just FStandalone }
      & help "Set feed rate"
  , defGCD { defCls = Just SStandalone }
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

-- 3D printer specific

groupExtruder :: [GCodeDef]
groupExtruder = makeGroup Extruder [
    m 82 ExtruderAbsolute
      & help "Interpret extrusion parameters as absolution positions"
  , m 83 ExtruderRelative
      & help "Interpret extrusion parameters as relative positions"
  ]

groupHeating :: [GCodeDef]
groupHeating = makeGroup Heating [
    m 104 SetExtruderTemperature
      & help "Set extruder temperature"
  , m 105 GetExtruderTemperature
      & help "Get current temperature of the selected extruder"
  , m 109 SetExtruderTemperatureAndWait
      & help "Set extruder temperature and wait for it to be reached"
  , m 140 SetBedTemperature
      & help "Set temperature of the heated bed"
  , m 190 SetBedTemperatureAndWait
      & help "Set heated bed temperature and wait for it to be reached"
  , m 141 SetChamberTemperature
      & help "Set temperature of the heated chamber"
  , m 191 SetChamberTemperatureAndWait
      & help "Set heated chamber and wait for it to be reached"
  , m 108 CancelWaitTemperature
      & help ("Stops waiting for temperature to be reached issued by M109, M190 or M191."
          ++ " This won't disable heaters and will continue the print job.")
  ]

groupCooling :: [GCodeDef]
groupCooling = makeGroup Cooling [
    m 106 FanOn
      & help "Enable fan"
  , m 107 FanOff
      & help "Disable fan"
  ]

groupPrinterMisc :: [GCodeDef]
groupPrinterMisc = makeGroup PrinterMisc [
    g 29 AutoBedLevel
      & help "Run automatic heated bed leveling"
  , m 84 DisableActuators
      & help "Disable actuators, e.g. cut power to steppers"
  , m 114 GetCurrentPosition
      & help "Report current position of all axes and extruders"
  , m 117 DisplayMessage
      & help "Display a text message on LCD display"
  ]

cncGroups :: [(RS274Group, [GCodeDef])]
cncGroups = [
    (Motion           , groupMotion)
  , (Plane            , groupPlane)
  , (Units            , groupUnits)
  , (ToolLengthOffset , groupToolLengthOffset)
  , (Cycles           , groupCycles)
  , (Distance         , groupDistance)
  , (ArcDistance      , groupArcDistance)
  , (FeedRateMode     , groupFeedRateMode)
  , (SpindleControl   , groupSpindleControl)
  , (Stopping         , groupStopping)
  , (CoolantControl   , groupCoolantControl)
  , (CutterRadius     , groupCutterRadius)
  , (LatheDiameterMode, groupLatheDiameterMode)
  , (OtherModal       , groupOtherModal)
  , (NonModal         , groupNonModal)
  ]

printerGroups :: [(RS274Group, [GCodeDef])]
printerGroups = [
    (Extruder    , groupExtruder)
  , (Heating     , groupHeating)
  , (Cooling     , groupCooling)
  , (PrinterMisc , groupPrinterMisc)
  ]

allGroups :: [(RS274Group, [GCodeDef])]
allGroups = cncGroups ++ printerGroups

groupNames :: [RS274Group]
groupNames = map fst allGroups

-- | All `GCodeDef`s known to us
allCodes :: [GCodeDef]
allCodes = concatMap snd allGroups
