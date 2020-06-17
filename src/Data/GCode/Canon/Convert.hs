module Data.GCode.Canon.Convert where

import Control.Applicative

import Data.GCode.Types (Code(..), Class(..), Axes, ParamDesignator(..))
import Data.GCode.Canon (Canon(..))

import qualified Data.Map
import qualified Data.GCode.Canon as C
import qualified Data.GCode.Types as T

import Data.GCode.RS274
import Data.GCode.Utils

-- | Convert code to its canonical representation
toCanon :: Code -> [Canon]
toCanon c | isRapid c =
    ifHasParam F c C.SetTraverseRate
 <> ifNonEmptyAxes c C.StraightTraverse
toCanon c | isMove c =
    ifHasParam F c C.SetFeedRate
 <> ifNonEmptyAxes c C.StraightFeed

-- :((
--toCanon c | isArc c = ArcFeed

toCanon c | isCoordinateSystemOffset c = pure $ C.SetCoords (codeAxes c)

toCanon c | isDwell c
  = pure . C.PauseSeconds $ getParamOrFail P c "No P for Dwell"

-- Converted by step
toCanon c | isMillimeters c = empty
toCanon c | isInches c      = empty
toCanon c | isAbsolute c    = empty
toCanon c | isRelative c    = empty

-- Planes
toCanon c | isXYPlane c = pure $ C.PlaneSelect C.XY
toCanon c | isZXPlane c = pure $ C.PlaneSelect C.ZX
toCanon c | isYZPlane c = pure $ C.PlaneSelect C.YZ
toCanon c | isUVPlane c = pure $ C.PlaneSelect C.UV
toCanon c | isWUPlane c = pure $ C.PlaneSelect C.WU
toCanon c | isVWPlane c = pure $ C.PlaneSelect C.VW

-- Standalone
toCanon Code { codeCls = Just FStandalone, codeNum = Just newFeed }
  = pure $ C.SetFeedRate $ fromIntegral newFeed
toCanon Code { codeCls = Just SStandalone, codeNum = Just spindleRPM }
  = pure $ C.SpindleSpeed $ fromIntegral spindleRPM

-- Units
toCanon c | isUnitsPerMinute c     = empty
toCanon c | isUnitsPerRevolution c = error "Don't know how to handle units per revolution"

-- Spindle
toCanon c | isSpindleCW c = pure C.SpindleStart
  { spindleDirection = C.ClockWise
  , spindleWaitForSpeed = True } -- questionable
toCanon c | isSpindleCCW c = pure C.SpindleStart
  { spindleDirection = C.CounterClockWise
  , spindleWaitForSpeed = True } -- questionable
toCanon c | isSpindleStop c = pure C.SpindleStop

-- Coolant
toCanon c | isCoolantMist  c = pure C.CoolantMist
toCanon c | isCoolantFlood c = pure C.CoolantFlood
toCanon c | isCoolantStop  c = pure C.CoolantStop

-- Tool
toCanon c | isToolChange c = pure C.ToolChange
toCanon (Code{codeCls=(Just T), codeNum=(Just toolId)}) = pure $ C.ToolSelect toolId
toCanon c | isToolLength c        = pure $ C.ToolLengthCompensation C.LengthTable
toCanon c | isToolLengthDynamic c = pure $ C.ToolLengthCompensation $ C.Dynamic
  (codeAxes c)
toCanon c | isToolLengthAdd c     = pure $ C.ToolLengthCompensation $ C.Add
  (round $ getParamOrFail H c "Add tool change offset requires H parameter of the tool to grab offset from")
toCanon c | isToolLengthCancel c  = pure $ C.ToolLengthCompensation C.NoCompensation

-- Printer -- XXX: needs handling in step
toCanon c | isExtruderAbsolute c = empty
toCanon c | isExtruderRelative c = empty

-- Printer heating
toCanon c | isSetExtruderTemperature c = pure $ C.SetTemperature
  (C.Heater C.HeatedExtruder $ round <$> getParam P c)
  (getParamOrFail S c "Set extruder temperature command missing S parameter for temperature value")
toCanon c | isSetBedTemperature c = pure $ C.SetTemperature
  (C.Heater C.HeatedBed $ round <$> getParam P c)
  (getParamOrFail S c "Set bed temperature command missing S parameter for temperature value")
toCanon c | isSetChamberTemperature c = pure $ C.SetTemperature
  (C.Heater C.HeatedChamber $ round <$> getParam P c)
  (getParamOrFail S c "Set heated chamber temperature command missing S parameter for temperature value")
toCanon c | isCancelWaitTemperature c = pure $ C.CancelWaitTemperature
-- Wait variants
toCanon c | isSetExtruderTemperatureAndWait c = pure $ C.SetTemperatureWait
  (C.Heater C.HeatedExtruder $ round <$> getParam P c)
  (getParamOrFail S c "Set extruder temperature and wait command missing S parameter for temperature value")
toCanon c | isSetBedTemperatureAndWait c = pure $ C.SetTemperatureWait
  (C.Heater C.HeatedBed $ round <$> getParam P c)
  (getParamOrFail S c "Set bed temperature and wait command missing S parameter for temperature value")
toCanon c | isSetChamberTemperatureAndWait c = pure $ C.SetTemperatureWait
  (C.Heater C.HeatedChamber $ round <$> getParam P c)
  (getParamOrFail S c "Set chamber temperature and wait command missing S parameter for temperature value")
-- Cancel
toCanon c | isCancelWaitTemperature c = pure $ C.CancelWaitTemperature

-- Printer cooling
toCanon c | isFanOn c = pure C.FanOn
toCanon c | isFanOff c = pure C.FanOff

-- Printer homing, XXX: this clashes with G28 of cnc which is StoredPositionMove
toCanon c | isGN 28 c = empty

-- Printer leveling
toCanon c | isAutoBedLevel c = pure C.LevelBed

-- Printer miscs, XXX: we probably can't even parse M117 Hello world
toCanon c | isDisplayMessage c = empty
toCanon c | isDisableActuators c = pure $ C.DisableMotors (codeAxes c)

toCanon c | isProgramEnd c = pure C.ProgramEnd
toCanon c | isCommentOnly c = pure $ C.Comment (codeComment c) -- XXX: strip spaces
toCanon (T.Comment c) = pure $ C.Comment c
toCanon Empty     = empty
toCanon (Other _) = empty -- questionable
-- this is bad but we can't use GHC to tell us about missing clauses
-- due to how Code type is freeform-ish (which is also the reason for Canon).
-- Lets stay on the safe side and error for now as ignoring could lead to
-- missing important commands.
toCanon c = error $ "No canon for " ++ show c

-- Helpers

-- Apply @f@ to parameter value only iff @p@ parameter is found, mempty otherwise
ifHasParam :: (Monoid (f a), Applicative f)
           => ParamDesignator
           -> Code
           -> (Double -> a)
           -> f a
ifHasParam p c f = case getParam p c of
  Nothing -> mempty
  Just val -> pure $ f val

-- Apply @f@ to `Axes` value only iff `Code` has axes, mempty otherwise
ifNonEmptyAxes :: (Applicative f, Monoid (f a))
               => Code
               -> (Axes -> a)
               -> f a
ifNonEmptyAxes c f | codeAxes c /= mempty = pure $ f (codeAxes c)
ifNonEmptyAxes _ _ | otherwise = mempty

-- Get parameter value or fail with `error`, useful for required parameters
getParamOrFail :: ParamDesignator
               -> Code
               -> [Char]
               -> Double
getParamOrFail param code msg = maybe (error msg) id (getParam param code)

-- brr
isCommentOnly :: Code -> Bool
isCommentOnly (Code { codeCls = Nothing
                    , codeNum = Nothing
                    , codeSub = Nothing
                    , codeAxes = a
                    , codeParams = p
                    , codeComment = x }) |
                      Data.Map.null a && Data.Map.null p &&
                      x /= mempty = True
isCommentOnly _ = False


