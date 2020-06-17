{-| GCode evaluator

Evaluates RS274 GCode

-}
{-# LANGUAGE RecordWildCards #-}
module Data.GCode.Eval where

import Control.Applicative
import Data.Maybe
import Data.Monoid
import qualified Data.Map.Strict as M

import Data.GCode.Types
import Data.GCode.RS274
import Data.GCode.RS274.Types
import Data.GCode.Utils
import Data.GCode.Canon

-- | Test if 'Code' belongs to group g
inGroup c g = any (\x -> x c) g

-- | Test if 'Code' belongs to any group
known c = M.member (decimate c) codesToDefs

-- | Update modal groups according to Code `c`
updateModals current c = case M.lookup (decimate c) codesToGroups of
  Nothing -> current
  Just group -> M.insert group c current

-- | Append axes from `cfrom` to `cto` Code
appendAxes cto cfrom = cto & (axes $ appendOnlyAxes (codeAxes cto) (codeAxes cfrom))

-- | Walk GCode adding missing axes coordinates according to previous moves
--
-- For example
-- G0 X1
-- G0 Y2
-- G0 Z3
--
-- becomes
-- G0 X1
-- G0 X1 Y2
-- G0 X1 Y2 Z3
--
-- also
--
-- G0 X1
-- Y2 Z2
--
-- becomes
--
-- G0 X1
-- G0 X1 Y2 Z2
totalize :: GCode -> GCode
totalize = totalize' defaultModals
  where
    totalize' modals [] = []
    totalize' modals (x:rest) =
      let (newCode, newModals) = updateCodeAndModals x modals
      in (newCode:totalize' newModals rest)

-- | Interpreter state
data IPState = IPState {
    ipModalGroups :: M.Map RS274Group Code
  , ipPosition :: Axes
  , ipLine :: Integer
  } deriving (Eq, Show, Ord)

-- | Default modals
defaultModals = M.fromList [
    (Units      , millimeters)
  , (Distance   , absolute)
  , (ArcDistance, absolute)
  ]

-- | Create new interpreter state
newState = IPState {
    ipModalGroups = defaultModals
  , ipPosition = M.empty
  , ipLine = 0
  }

-- |Convert code to its canonical representation
toCanon :: Code -> Canon
toCanon c | isRapid c = StraightTraverse (codeAxes c)
toCanon c | isMove c = StraightFeed (codeAxes c)
--toCanon c | isArc c = ArcFeed
toCanon c | isDwell c = case getParam P c of
  Nothing -> error "No P for dwell"
  Just p -> PauseSeconds p
toCanon c | isProgramEnd c = Stop
toCanon c = error $ "No canon for " ++ show c
 
-- |Same as toCanon but result is wrapped in `LineNumbered`
-- according to current interpreter line
toCanonLines :: Code -> IPState -> LineNumbered Canon
toCanonLines c is = Line (ipLine is) $ toCanon c


step :: IPState -> GCode -> (Maybe Code, IPState, GCode)
step is [] = (Nothing, is, [])
step is@IPState{..} (x@Code{}:xs) =
  let (newCode, newModals) = updateCodeAndModals x ipModalGroups
      -- update position with new codeAxes
      newPosition = updateAxes ipPosition (codeAxes newCode)
  in (Just $ newCode
      , is { ipModalGroups = newModals
         , ipPosition = newPosition
         , ipLine = ipLine + 1 }
      , xs)
-- handle empty/comments/other
step is (x:xs) = (Nothing, is, xs)

-- | Fully evaluate GCode
eval :: GCode -> ([Code], IPState)
eval = evalWith (\res state -> res)

-- | Evaluate GCode to canonnical representation
evalCanon :: GCode -> ([LineNumbered Canon], IPState)
evalCanon = evalWith toCanonLines

-- | Evaluate GCode and and apply function `f` to each successfuly
-- evaluated Code
evalWith :: (Code -> IPState -> a) -> GCode -> ([a], IPState)
evalWith f gcode = let (accumulator, resultState, []) = go initState in (catMaybes accumulator, resultState)
  where
    initState = ([], newState, gcode)
    go x@(_, _, []) = x
    go x@(acc, st, codes) =
      let (result, steppedState, rest) = step st codes
          mapped = case result of
            Nothing -> Nothing
            Just x -> f x steppedState
      in go (acc ++ [mapped], steppedState, rest)

-- | Evaluate GCode and return each evaluation step
evalSteps gcode = go initState
  where
    initState = ([], newState, gcode)
    go x@(_, _, []) = [x]
    go x@(acc, st, codes) = let (result, steppedState, rest) = step st codes in x:(go (result:acc, steppedState, rest))

-- interpreter *always* runs
-- * in absolute mode
-- * with millimeters as units
-- * with total commands in modal groups
-- convert accordingly!
-- | Convert all axis coordinates from inches to millimeters if needed
toMillimeters modals x | codeActive millimeters modals = x
toMillimeters modals x | codeActive inches modals = x & axes (M.map (*25.4) (codeAxes x))
                                                      & modifyParams [F, R, I, J, K] (*25.4)
toMillimeters modals x | otherwise = error "Neither millimeters nor inches set"

-- | Convert all motion coordinates from relative to absolute
toAbsolute modals x | codeActive relative modals && isMotion x =
  case M.lookup Motion modals of -- motion group
    Nothing -> x
    (Just e) -> x & (axes $ addRelative (codeAxes x) (codeAxes e))
  where
    addRelative :: Axes -> Axes -> Axes
    addRelative existing new = M.unionWith (+) existing new
toAbsolute modals x | otherwise = x

-- | Convert all arc coordinates from relative to absolute
toAbsoluteArcs modals x | codeActive arcRelative modals && isMotion x =
  case M.lookup Motion modals of -- motion group
    Nothing -> x
    (Just e) -> x & modifyParamsWithKey [I, J, K] (addRespective e)
  where
    addRespective code I x | hasAxis X code = fromJust (getAxis X code) + x
    addRespective code J x | hasAxis Y code = fromJust (getAxis Y code) + x
    addRespective code K x | hasAxis Z code = fromJust (getAxis Z code) + x
    addRespective _    _ x | otherwise      = x
toAbsoluteArcs modals x | otherwise = x

-- | Return True if `code` is active (present) in `modals`
codeActive code modals = case M.lookup (decimate code) codesToGroups of
  Just group -> M.lookup group (M.map decimate modals) == (Just $ decimate code)
  Nothing -> False

-- | Return True if `code` is a motion comand
isMotion = flip codeInGroup Motion

-- | Update `code` according to current `modals`
-- then update `modals` with a resulting code
--
-- Return updated code and modals
updateCodeAndModals code modals =
      -- first we update current GCode with missing data
  let newCode = updateFromCurrentModals modals
              $ updateIncompleteFromCurrentModals modals
              $ toAbsoluteArcs modals
              $ toAbsolute modals
              $ toMillimeters modals code
      -- then we update stored modal groups with updated GCode
      newModals = updateModals modals newCode
  in (newCode, newModals)

-- | Take current motion group modal code and update this motion code
-- with missing coordinates of the stored one
updateFromCurrentModals modals x | isMotion x = do
  case M.lookup Motion modals of -- motion group
    Nothing -> x
    (Just e) -> x & (axes $ appendOnlyAxes (codeAxes x) (codeAxes e))
updateFromCurrentModals _ x | otherwise = x

-- | Return True if this code contains only coordinates
incomplete Code{codeCls=Nothing, codeNum=Nothing, ..} | (M.null codeAxes /= True) = True
incomplete _ = False

-- | Update incomplete motion Code with the stored one
updateIncompleteFromCurrentModals modals x | incomplete x = do
  case M.lookup Motion modals of -- motion group
    Nothing -> x
    (Just e) -> appEndo (mconcat $ map Endo [
        (cls $ fromJust $ codeCls e)
      , (num $ fromJust $ codeNum e)
      , (axes $ appendOnlyAxes (codeAxes x) (codeAxes e))
      ]) x
updateIncompleteFromCurrentModals _ x | otherwise = x

-- | Update axes that aren't defined in target
appendOnlyAxes target from = M.union target missingOnly
  where missingOnly = M.difference from target

-- | Update (replace) `target` axes with `from` axes
updateAxes target from = M.union from target -- union in this order so `from` axes are preferred

-- | Update `Limits` from this `Code`
updateLimitsCode :: Limits -> Code -> Limits
updateLimitsCode s Code{..} = updateLimits s codeAxes
updateLimitsCode s _ = s

-- | Update `Limits` from `Axes`
updateLimits :: Limits -> Axes -> Limits
updateLimits s = M.foldlWithKey adj s
  where
    adj limits ax val = M.alter (alterfn val) ax limits
    alterfn val (Just (min_c, max_c)) = Just (min min_c val, max max_c val)
    alterfn val Nothing = Just (val, val)
