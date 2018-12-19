{-| GCode pretty-printing functions

Utilities for manipulating and filtering 'GCode'

-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
module Data.GCode.Utils where

import Debug.Trace
import Data.Maybe
import Data.Monoid

import Data.GCode.Types
import qualified Data.Map.Strict as M

import Control.Monad.State.Strict
import Control.Applicative

-- |True if 'Code' is a G-code
isG :: Code -> Bool
isG Code{codeCls=(Just G), ..} = True
isG _ = False

-- |True if 'Code' is a M-code
isM :: Code -> Bool
isM Code{codeCls=(Just M), ..} = True
isM _ = False

-- |True if 'Code' is a G{N} code
isGN :: Int -> Code -> Bool
isGN n Code{codeCls=(Just G), codeNum=(Just x), ..} = x == n
isGN _ _ = False

-- |True if 'Code' is a G{N}.{sub} code
isGNs n sub Code{codeCls=(Just G), codeNum=(Just x), codeSub=(Just sx), ..} = x == n && sx == sub
isGNs _ _ _ = False

-- |True if 'Code' is a M{N} code
isMN :: Int -> Code -> Bool
isMN n Code{codeCls=(Just M), codeNum=(Just x), ..} = x == n
isMN _ _ = False

-- |True if 'Code' is a M{N}.{sub} code
isMNs n sub Code{codeCls=(Just M), codeNum=(Just x), codeSub=(Just sx), ..} = x == n && sx == sub
isMNs _ _ _ = False

-- |True if 'Code' is a G0 code
isG0 :: Code -> Bool
isG0 = isGN 0

-- |True if 'Code' is a G0 (rapid move) code, alias to 'isG0'
isRapid :: Code -> Bool
isRapid = isG0

-- |True if 'Code' is a G1 code
isG1 :: Code -> Bool
isG1 = isGN 1

-- |True if 'Code' is a G1 (move) code, alias to 'isG1'
isMove :: Code -> Bool
isMove = isG1

-- |True if 'Code' is a G2 code
isG2 :: Code -> Bool
isG2 = isGN 2

-- |True if 'Code' is a G2 (clockwise circular move) code, alias to 'isG2'
isArcCW :: Code -> Bool
isArcCW = isG2

-- |True if 'Code' is a G3 code
isG3 :: Code -> Bool
isG3 = isGN 3

-- |True if 'Code' is a G3 (counter-clockwise circular move) code, alias to 'isG3'
isArcCCW :: Code -> Bool
isArcCCW = isG3

-- |True if 'Code' is a G4 code
isG4 :: Code -> Bool
isG4 = isGN 4

-- |True if 'Code' is a G4 (dwell) code, alias to 'isG4'
isDwell :: Code -> Bool
isDwell = isG4

-- |True if 'Code' is a G5 code
isG5 :: Code -> Bool
isG5 = isGN 5

-- |True if 'Code' is a G5 (cubic spline) code, alias to 'isG5'
isCubicSpline :: Code -> Bool
isCubicSpline = isG5

-- |True if 'Code' is a G5.1 code
isG5s1 :: Code -> Bool
isG5s1 = isGNs 5 1

-- |True if 'Code' is a G5.1 (quadratic spline) code, alias to 'isG5s1'
isQuadSpline :: Code -> Bool
isQuadSpline = isG5s1

-- |True if 'Code' is a G5.2 code
isG5s2 :: Code -> Bool
isG5s2 = isGNs 5 2

-- |True if 'Code' is a G5.2 (NURBS) code, alias to 'isG5s2'
isNURBS :: Code -> Bool
isNURBS = isG5s2

-- |True if 'Code' is a G17 (select XYZ plane) code
isXYZplane :: Code -> Bool
isXYZplane = isGN 17

-- |True if 'Code' is a G18 (select XZY plane) code
isXZYplane :: Code -> Bool
isXZYplane = isGN 18

-- |True if 'Code' is a G19 (select YZX plane) code
isYZXplane :: Code -> Bool
isYZXplane = isGN 19

groupPlane = [ isXYZplane, isXZYplane, isYZXplane ]

-- |True if 'Code' is a G20 (inch mode) code
isInch :: Code -> Bool
isInch = isGN 20

-- |True if 'Code' is a G21 (millimeter mode) code
isMM :: Code -> Bool
isMM = isGN 21

groupUnits = [ isInch, isMM ]

-- |True if 'Code' is a G33 code
isG33 :: Code -> Bool
isG33 = isGN 33

-- |True if 'Code' is a G33 (spindle synchronized motion) code, alias to 'isG33'
isSpindleSync :: Code -> Bool
isSpindleSync = isG33

-- |True if 'Code' is a G33.1 code
isG33s1 :: Code -> Bool
isG33s1 = isGNs 33 1

-- |True if 'Code' is a G33.1 (rigit tapping) code, alias to 'isG33s1'
isRigidTap :: Code -> Bool
isRigidTap = isG33s1

-- |True if 'Code' is a G38 code
isG38 :: Code -> Bool
isG38 = isGN 38

-- |True if 'Code' is a G38 (probe) code, alias to 'isG38'
isProbe :: Code -> Bool
isProbe = isG38

groupMotion = [isMove, isRapid, isArcCW, isArcCCW,
  isCubicSpline, isQuadSpline, isNURBS, isProbe, isSpindleSync, isRigidTap]

-- |True if 'Code' is a G73 (drilling cycle, chip breaking) code
isDrillingCycleCB :: Code -> Bool
isDrillingCycleCB = isGN 73

-- |True if 'Code' is a G76 (threading cycle) code
isThreadingCycle :: Code -> Bool
isThreadingCycle = isGN 76

-- |True if 'Code' is a G80 (cancel drilling cycle) code
isDrillingCycleCancel :: Code -> Bool
isDrillingCycleCancel = isGN 80

-- |True if 'Code' is a G81 (drilling cycle) code
isDrillingCycle :: Code -> Bool
isDrillingCycle = isGN 81

-- |True if 'Code' is a G82 (drilling cycle, dwell) code
isDrillingCycleDwell :: Code -> Bool
isDrillingCycleDwell = isGN 82

-- |True if 'Code' is a G83 (drilling cycle, pecky) code
isDrillingCyclePeck :: Code -> Bool
isDrillingCyclePeck = isGN 83

-- |True if 'Code' is a G85 (boring cycle, feed out) code
isBoringCycle :: Code -> Bool
isBoringCycle = isGN 85

-- |True if 'Code' is a G89 (boring cycle, dwell, feed out) code
isBoringCycleDwell :: Code -> Bool
isBoringCycleDwell = isGN 89

groupCycles = [isDrillingCycle, isDrillingCycleCB, isDrillingCyclePeck,
  isDrillingCycleDwell, isDrillingCycleCancel,
  isThreadingCycle,
  isBoringCycle, isBoringCycleDwell ]

-- |True if 'Code' is a G90 (absolute mode) code
isAbsolute :: Code -> Bool
isAbsolute = isGN 90

-- |True if 'Code' is a G91 (relative mode) code
isRelative :: Code -> Bool
isRelative = isGN 91

-- |True if 'Code' is a G90.1 (absolute arc mode) code
isArcAbsolute :: Code -> Bool
isArcAbsolute = isGNs 90 1

-- |True if 'Code' is a G91.1 (relative arc mode) code
isArcRelative :: Code -> Bool
isArcRelative = isGNs 91 1

-- |True if 'Code' is a G7 (lathe diameter mode) code
isLatheDiameter :: Code -> Bool
isLatheDiameter = isGN 7

-- |True if 'Code' is a G8 (lathe radius mode) code
isLatheRadius :: Code -> Bool
isLatheRadius = isGN 8

groupDistance = [ isAbsolute, isRelative,
  isArcAbsolute, isArcRelative, isLatheDiameter, isLatheRadius ]

--isModal = isModalGMotion || isModalPlane || isModalUnits || isModalAbsRel

-- |True if 'Code' is a G93 (inverse time mode) code
isInverseTime :: Code -> Bool
isInverseTime = isGN 93

-- |True if 'Code' is a G94 (units per minute time mode) code
isUnitsPerMinute :: Code -> Bool
isUnitsPerMinute = isGN 94

-- |True if 'Code' is a G95 (units per revolution time mode) code
isUnitsPerRevolution :: Code -> Bool
isUnitsPerRevolution = isGN 95

groupFeedRateMode = [ isInverseTime, isUnitsPerMinute, isUnitsPerRevolution ]

-- |True if 'Code' is a M3 (spindle start clockwise) code
isSpindleCW :: Code -> Bool
isSpindleCW = isMN 3

-- |True if 'Code' is a M4 (spindle start counter-clockwise) code
isSpindleCCW :: Code -> Bool
isSpindleCCW = isMN 4

-- |True if 'Code' is a M5 (spindle stop) code
isSpindleStop :: Code -> Bool
isSpindleStop = isMN 5

groupSpindleControl = [ isSpindleCW, isSpindleCCW, isSpindleStop,
  isMN 19, isGN 96, isGN 97]

-- |True if 'Code' is a M7 (turn mist coolant on) code
isCoolantMist :: Code -> Bool
isCoolantMist = isMN 7

-- |True if 'Code' is a M8 (turn flood coolant on) code
isCoolantFlood :: Code -> Bool
isCoolantFlood = isMN 8

-- |True if 'Code' is a M9 (turn all coolant off) code
isCoolantStop :: Code -> Bool
isCoolantStop = isMN 9

groupCoolantControl = [ isCoolantMist, isCoolantFlood, isCoolantStop ]

-- |True if 'Code' is a G43 (tool length offset) code
isToolLength :: Code -> Bool
isToolLength = isGN 43

-- |True if 'Code' is a G43.1 (dynamic tool length offset) code
isToolLengthDynamic :: Code -> Bool
isToolLengthDynamic = isGNs 43 1

-- |True if 'Code' is a G43.2 (apply additional tool length offset) code
isToolLengthAdd :: Code -> Bool
isToolLengthAdd = isGNs 43 2

-- |True if 'Code' is a G49 (cancel tool length offset) code
isToolLengthCancel :: Code -> Bool
isToolLengthCancel = isGN 49

groupToolLengthOffset = [ isToolLength, isToolLengthDynamic,
  isToolLengthAdd, isToolLengthCancel ]

-- |True if 'Code' is a M0 (pause) code
isPause :: Code -> Bool
isPause = isMN 0

-- |True if 'Code' is a M1 (optional pause) code
isOptionalPause :: Code -> Bool
isOptionalPause = isMN 1

-- |True if 'Code' is a M2 (program end) code
isEnd :: Code -> Bool
isEnd = isMN 2

-- |True if 'Code' is a M30 (exchange pallet shuttles) code
isExchange :: Code -> Bool
isExchange = isMN 30

groupStopping = [ isPause, isOptionalPause, isEnd, isExchange, isMN 60 ]

groups = [ groupMotion, groupCycles, groupDistance, groupFeedRateMode,
  groupSpindleControl, groupCoolantControl, groupStopping, groupUnits,
  groupPlane ]

-- |True if 'Code' has a coordinate in axis 'a'
hasAxis :: AxisDesignator -> Code -> Bool
hasAxis a Code{..} = M.member a codeAxes
hasAxis a _ = False

getAxis :: AxisDesignator -> Code -> Maybe Double
getAxis a Code{..} = M.lookup a codeAxes
getAxis _ _ = Nothing

getAxes :: [AxisDesignator] -> Code -> [Maybe Double]
getAxes as c@Code{..} = map (\a-> getAxis a c) as
getAxes _ _ = []

getAxesToList :: Code -> [(AxisDesignator, Double)]
getAxesToList Code{..} = M.toList codeAxes
getAxesToList _ = []

--filterAxes :: [AxisDesignator] -> Code -> [Double]
--filterAxes ax Code{..} = map (\a -> M.lookup a codeAxes) ax

-- |True if 'Code' contains 'X' axis
hasX :: Code -> Bool
hasX = hasAxis X

-- |True if 'Code' contains 'Y' axis
hasY :: Code -> Bool
hasY = hasAxis Y

-- |True if 'Code' contains 'Z' axis
hasZ :: Code -> Bool
hasZ = hasAxis Z

-- |True if 'Code' contains 'E' axis
hasE :: Code -> Bool
hasE = hasAxis E

-- |True if 'Code' contains parameter with 'ParamDesignator'
hasParam :: ParamDesignator -> Code -> Bool
hasParam p Code{..} = M.member p codeParams
hasParam a _ = False

getParam :: ParamDesignator -> Code -> Maybe Double
getParam p Code{..} = M.lookup p codeParams

-- |True if 'Code' contains feedrate parameter (e.g. G0 F3000)
hasFeedrate :: Code -> Bool
hasFeedrate = hasParam F

-- |Filter G-codes
gcodes :: [Code] -> [Code]
gcodes = filter isG

-- |Filter M-codes
mcodes :: [Code] -> [Code]
mcodes = filter isM

-- |Filter rapid moves
rapids :: [Code] -> [Code]
rapids = filter isRapid

-- |Filter moves
moves :: [Code] -> [Code]
moves  = filter isMove

-- |Replace 'Class' of 'Code' (e.g. for chaning G0 to M0)
replaceClass :: Class -> Code -> Code
replaceClass newclass c = appmod (cls newclass) c

-- |Replace code value of 'Code' (e.g. for chaning G0 to G1)
replaceCode :: Int -> Code -> Code
replaceCode newcode c = appmod (num newcode) c

-- |Replace axis with 'AxisDesignator' in 'Code' returning new 'Code'
replaceAxis :: AxisDesignator -> Double -> Code -> Code
replaceAxis de val c@Code{..} | hasAxis de c = addReplaceAxis de val c
replaceAxis _ _ c = c

modifyAxis :: AxisDesignator -> (Double -> Double) -> Code -> Code
modifyAxis de f c@Code{..} | hasAxis de c = addReplaceAxis de (f $ fromJust $ getAxis de c) c
modifyAxis _ _ c = c

modifyAxes :: [AxisDesignator] -> (Double -> Double) -> Code -> Code
modifyAxes axes f c = foldl (\c1  ax -> modifyAxis ax f c1) c axes

hasXY c = hasAxis X c && hasAxis Y c

modifyXY :: (Double -> Double -> (Double, Double)) -> Code -> Code
modifyXY f c | hasXY c =
  let x = fromJust $ getAxis X c
      y = fromJust $ getAxis Y c
      (nx, ny) = f x y
  in appmod (axis X nx <> axis Y ny) c
modifyXY _ c = c

-- |Replace or add axis with 'AxisDesignator' in 'Code' returning new 'Code'
addReplaceAxis :: AxisDesignator -> Double -> Code -> Code
addReplaceAxis de val c@Code{..} = appmod (axes $ newaxes $ codeAxes) c
  where
    newaxes = M.insert de val
addReplaceAxis _ _ x = x

-- |Replace X axis coordnate
replaceX :: Double -> Code -> Code
replaceX = replaceAxis X

-- |Replace Y axis coordinate
replaceY :: Double -> Code -> Code
replaceY = replaceAxis Y

-- |Replace Z axis coordinate
replaceZ :: Double -> Code -> Code
replaceZ = replaceAxis Z

-- |Replace E axis coordinate
replaceE :: Double -> Code -> Code
replaceE = replaceAxis E

-- |Replace or add X axis coordinate
addReplaceX :: Double -> Code -> Code
addReplaceX = addReplaceAxis X

-- |Replace or add Y axis coordinate
addReplaceY :: Double -> Code -> Code
addReplaceY = addReplaceAxis Y

-- |Replace or add Z axis coordinate
addReplaceZ :: Double -> Code -> Code
addReplaceZ = addReplaceAxis Z

-- |Replace or add E axis coordinate
addReplaceE :: Double -> Code -> Code
addReplaceE = addReplaceAxis E


-- |Replace parameter with 'ParamDesignator' in 'Code' returning new 'Code'
replaceParam :: ParamDesignator -> Double -> Code -> Code
replaceParam de val c@Code{..} | hasParam de c = addReplaceParam de val c
replaceParam _ _ c = c

modifyParam :: ParamDesignator -> (Double -> Double) -> Code -> Code
modifyParam de f c@Code{..} | hasParam de c = addReplaceParam de (f $ fromJust $ getParam de c) c
modifyParam _ _ c = c

-- |Replace or add parameter with 'ParamDesignator' in 'Code' returning new 'Code'
addReplaceParam :: ParamDesignator -> Double -> Code -> Code
addReplaceParam de val c@Code{..} = appmod (params $ newparams $ codeParams) c
  where
    newparams = M.insert de val
addReplaceParam _ _ x = x

-- |Replace feedrate (F parameter) in 'Code' returning new 'Code'
replaceFeedrate :: Double -> Code -> Code
replaceFeedrate = replaceParam F

modifyFeedrate :: (Double -> Double) -> Code -> Code
modifyFeedrate = modifyParam F

-- |Sum of all axis distances of this 'Code'
travel :: Code -> Double
travel Code{codeCls=(Just G), ..} = M.foldl (+) 0 codeAxes
travel _ = 0

-- |Test if 'Code' belongs to group g
inGroup c g = any (\x -> x c) g

-- |Test if 'Code' belongs to any group
known c = any (\x -> inGroup c x) groups

--updateModals current c = trace (show $ zipWith (,) current $ map (\x -> inGroup c x) groups) $ zipWith maybeUpdate current $ map (\x -> inGroup c x) groups
--  where
--    maybeUpdate Nothing True = trace ("new modal" ++ show c) $ Just c
--    maybeUpdate (Just old) True = trace ("change modal" ++ show (appendAxes c old)) $ Just (appendAxes c old)
--    maybeUpdate old False = trace ("no match" ++ show old) $ old
--
updateModals current c = zipWith maybeUpdate current $ map (\x -> inGroup c x) groups
  where
    maybeUpdate Nothing True = Just c
    maybeUpdate (Just old) True =  Just (appendAxes c old)
    maybeUpdate old False =  old

appendAxes cto cfrom = appmod (axes $ appendOnlyAxes (codeAxes cto) (codeAxes cfrom)) cto

incomplete Code{codeCls=Nothing, ..} = True
incomplete Code{codeNum=Nothing, ..} = True
incomplete _ = False

totalize :: GCode -> GCode
totalize = totalize' emptyGroups
  where
    totalize' inEffect [] = []
    totalize' inEffect (x:rest) = (updateFromEffect inEffect x):(totalize' (updateModals inEffect x) rest)


type Evaluator a = State [Maybe Code] a

totalizer :: GCode -> Evaluator GCode
totalizer [] = return $ []
totalizer (x:xs) = do
  trace ("x " ++ show x) $ return ()
  cs <- get
  let nx = updateFromEffect cs x

  modify' (flip updateModals $ nx)

  --trace ("cs " ++ show cs) $ return ()
  --trace ("nx " ++ show nx) $ return ()
  rest <- totalizer xs

  return $ (nx:rest)

totalize' c = runState (totalizer c) emptyGroups

--totalizeTrace' c = do
--  (a, b) <- runState (totalizer c) emptyGroups
--  return $ (a, b)

updateFromEffect inEffect x = do
  case (!!) inEffect 0 of -- motion group
    Nothing -> x
    (Just e) -> appmod (
         (cls $ fromJust $ codeCls e)
      <> (num $ fromJust $ codeNum e)
      <> (axes $ appendOnlyAxes (codeAxes x) (codeAxes e))
      ) x

updateFromEffect _ x | otherwise = x

updateIncompleteFromEffect inEffect x | incomplete x = do
  case (!!) inEffect 0 of -- motion group
    Nothing -> x
    (Just e) -> appmod (
         (cls $ fromJust $ codeCls e)
      <> (num $ fromJust $ codeNum e)
      <> (axes $ appendOnlyAxes (codeAxes x) (codeAxes e))
      ) x

updateIncompleteFromEffect _ x | otherwise = x

emptyGroups = map (pure Nothing) groups

-- update axes that aren't defined in target
appendOnlyAxes target from = M.union target missingOnly
  where missingOnly = M.difference from target

rot by x y = (x * (cos by) - y * (sin by), y * (cos by) + x * (sin by))

roundprec n x = (fromInteger $ round $ x * (10^n)) / (10.0^^n)

updateLimitsCode :: Limits -> Code -> Limits
updateLimitsCode s Code{..} = updateLimits s codeAxes
updateLimitsCode s _ = s

updateLimits :: Limits -> Axes -> Limits
updateLimits s = M.foldlWithKey adj s
  where
    adj limits ax val = M.alter (alterfn val) ax limits
    alterfn val (Just (min_c, max_c)) = Just (min min_c val, max max_c val)
    alterfn val Nothing = Just (val, val)
