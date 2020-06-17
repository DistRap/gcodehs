{-| GCode evaluator

Evaluates RS274 GCode

-}
{-# LANGUAGE RecordWildCards #-}
module Data.GCode.Eval where

import Data.Maybe
import Data.Monoid
import Data.Map (Map)

import qualified Data.Map

import Data.GCode.Ann (Ann(SrcLine))
import Data.GCode.Types
import Data.GCode.RS274
import Data.GCode.RS274.Types
import Data.GCode.Utils
import Data.GCode.Canon (Canon)
import Data.GCode.Canon.Convert

-- | Interpreter state
data IPState = IPState {
    ipModalGroups :: Map RS274Group Code
  , ipPosition :: Axes
  , ipLine :: Integer
  } deriving (Eq, Show, Ord)

-- | Default modals
defaultModals :: Map RS274Group Code
defaultModals = Data.Map.fromList [
    (Units      , millimeters)
  , (Distance   , absolute)
  , (ArcDistance, absolute)
  ]

-- | Create new interpreter state
newState :: IPState
newState = IPState {
    ipModalGroups = defaultModals
  , ipPosition    = mempty
  , ipLine        = 0
  }

-- | Step `Code` interpreter
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
step is (_:xs) = (Nothing, is, xs)

-- | Evaluate GCode and return each evaluation step
evalSteps :: [Code] -> [([Maybe Code], IPState, [Code])]
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
toMillimeters :: Map RS274Group Code -> Code -> Code
toMillimeters modals x | codeActive millimeters modals = x
toMillimeters modals x | codeActive inches modals = x & axes (Data.Map.map (*25.4) (codeAxes x))
                                                      & modifyParams [F, R, I, J, K] (*25.4)
toMillimeters _ _      | otherwise = error "Neither millimeters nor inches set"

-- | Convert all motion coordinates from relative to absolute
toAbsolute :: Map RS274Group Code -> Code -> Code
toAbsolute modals x | codeActive relative modals && isMotion x =
  case Data.Map.lookup Motion modals of -- motion group
    Nothing -> x
    (Just e) -> x & (axes $ addRelative (codeAxes x) (codeAxes e))
  where
    addRelative :: Axes -> Axes -> Axes
    addRelative existing new = Data.Map.unionWith (+) existing new
toAbsolute _ x      | otherwise = x

-- | Convert all arc coordinates from relative to absolute
toAbsoluteArcs :: Map RS274Group Code -> Code -> Code
toAbsoluteArcs modals c | codeActive arcRelative modals && isMotion c =
  case Data.Map.lookup Motion modals of -- motion group
    Nothing -> c
    (Just e) -> c & modifyParamsWithKey [I, J, K] (addRespective e)
  where
    addRespective code I x | hasAxis X code = fromJust (getAxis X code) + x
    addRespective code J x | hasAxis Y code = fromJust (getAxis Y code) + x
    addRespective code K x | hasAxis Z code = fromJust (getAxis Z code) + x
    addRespective _    _ x | otherwise      = x
toAbsoluteArcs _ c     | otherwise = c

-- | Return True if `code` is active (present) in `modals`
codeActive :: Code -> Map RS274Group Code -> Bool
codeActive code modals = case Data.Map.lookup (decimate code) codesToGroups of
  Just group -> Data.Map.lookup group (Data.Map.map decimate modals) == (Just $ decimate code)
  Nothing -> False

-- | Return True if `code` is a motion comand
isMotion :: Code -> Bool
isMotion = flip codeInGroup Motion

-- | Update `code` according to current `modals`
-- then update `modals` with a resulting code
--
-- Return updated code and modals
updateCodeAndModals :: Code
                    -> Map RS274Group Code
                    -> (Code, Map RS274Group Code)
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

-- | Update modal groups according to Code `c`
updateModals :: Map RS274Group Code
             -> Code
             -> Map RS274Group Code
updateModals current c = case Data.Map.lookup (decimate c) codesToGroups of
  Nothing -> current
  Just group -> Data.Map.insert group c current

-- | Take current motion group modal code and update this motion code
-- with missing coordinates of the stored one
updateFromCurrentModals :: Map RS274Group Code -> Code -> Code
updateFromCurrentModals modals x | isMotion x = do
  case Data.Map.lookup Motion modals of -- motion group
    Nothing -> x
    (Just e) -> x & (axes $ appendOnlyAxes (codeAxes x) (codeAxes e))
updateFromCurrentModals _ x | otherwise = x

-- | Return True if this code contains only coordinates
incomplete :: Code -> Bool
incomplete Code{codeCls=Nothing, codeNum=Nothing, ..} | (Data.Map.null codeAxes /= True) = True
incomplete _ = False

-- | Update incomplete motion Code with the stored one
updateIncompleteFromCurrentModals :: Map RS274Group Code -> Code -> Code
updateIncompleteFromCurrentModals modals x | incomplete x = do
  case Data.Map.lookup Motion modals of -- motion group
    Nothing -> x
    (Just e) -> appEndo (mconcat $ map Endo [
        (cls $ fromJust $ codeCls e)
      , (num $ fromJust $ codeNum e)
      , (axes $ appendOnlyAxes (codeAxes x) (codeAxes e))
      ]) x
updateIncompleteFromCurrentModals _ x | otherwise = x

-- | Update axes that aren't defined in target
appendOnlyAxes :: Ord k => Map k b -> Map k b -> Map k b
appendOnlyAxes target from = Data.Map.union target missingOnly
  where missingOnly = Data.Map.difference from target

-- | Update (replace) `target` axes with `from` axes
updateAxes :: Ord k => Map k a -> Map k a -> Map k a
updateAxes target from = Data.Map.union from target -- union in this order so `from` axes are preferred

-- | Update `Limits` from this `Code`
updateLimitsCode :: Limits -> Code -> Limits
updateLimitsCode s Code{..} = updateLimits s codeAxes
updateLimitsCode s _ = s

-- | Update `Limits` from `Axes`
updateLimits :: Limits -> Axes -> Limits
updateLimits s = Data.Map.foldlWithKey adj s
  where
    adj limits ax val = Data.Map.alter (alterfn val) ax limits
    alterfn val (Just (min_c, max_c)) = Just (min min_c val, max max_c val)
    alterfn val Nothing = Just (val, val)

-- Slow evaluators for testing, use streaming variants from `Data.GCode.Pipes` instead.

-- | Fully evaluate GCode
eval :: GCode -> ([Code], IPState)
eval = evalWith (\res _state -> Just res)

-- | Evaluate GCode to canonical representation
evalToCanon :: GCode -> ([Canon], IPState)
evalToCanon = evalWith' (\c _ips -> toCanon c)

-- | Evaluate GCode to annotated canonnical representation
evalToCanonAnn :: GCode -> ([Ann Canon], IPState)
evalToCanonAnn = evalWith' toCanonAnn

-- | Same as toCanon but result is wrapped in `Ann`
-- according to current interpreter line
toCanonAnn :: Code -> IPState -> [Ann Canon]
toCanonAnn c is = SrcLine (ipLine is) <$> toCanon c

-- | Evaluate GCode and and apply function `f` to each successfuly
-- evaluated Code
--
-- Slow due to list concatenation, use streaming variants from `Data.GCode.Pipes` instead.
evalWith :: (Code -> IPState -> Maybe a)
         -> GCode
         -> ([a], IPState)
evalWith f gcode = let (accumulator, resultState, []) = go initState in (catMaybes accumulator, resultState)
  where
    initState = ([], newState, gcode)
    go x@(_, _, []) = x
    go   (acc, st, codes) =
      let (result, steppedState, rest) = step st codes
          mapped = case result of
            Nothing -> Nothing
            Just x -> f x steppedState
      in go (acc ++ [mapped], steppedState, rest)

-- Like `evalWith` but allows multiple elements to be generated
evalWith' :: (Code -> IPState -> [a])
         -> GCode
         -> ([a], IPState)
evalWith' f gcode =
  let (accumulator, resultState, []) = go initState
  in (accumulator, resultState)
  where
    initState = ([], newState, gcode)
    go x@(_, _, []) = x
    go   (acc, st, codes) =
      let (result, steppedState, rest) = step st codes
          mapped = case result of
            Nothing -> []
            Just r -> f r steppedState
      in go (acc ++ mapped, steppedState, rest)

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
    totalize' _ [] = []
    totalize' modals (x:rest) =
      let (newCode, newModals) = updateCodeAndModals x modals
      in (newCode:totalize' newModals rest)
