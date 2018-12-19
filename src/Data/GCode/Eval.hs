{-| GCode evaluator

Evaluates RS274 GCode

-}
{-# LANGUAGE RecordWildCards #-}
module Data.GCode.Eval where

import Control.Applicative
import Data.Maybe
import qualified Data.Map.Strict as M

import Data.GCode.Types
import Data.GCode.RS274
import Data.GCode.RS274.Types

-- |Test if 'Code' belongs to group g
inGroup c g = any (\x -> x c) g

-- |Test if 'Code' belongs to any group
known c = M.member (decimate c) codesToDefs

-- |Update modal groups according to Code `c`
updateModals current c = case M.lookup (decimate c) codesToGroups of
  Nothing -> current
  Just group -> M.insert group c current

-- |Append axes from `cfrom` to `cto` Code
appendAxes cto cfrom = appmod (axes $ appendOnlyAxes (codeAxes cto) (codeAxes cfrom)) cto

-- |Walk GCode adding missing axes coordinates according to previous moves
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
totalize :: GCode -> GCode
totalize = totalize' M.empty
  where
    totalize' inEffect [] = []
    totalize' inEffect (x:rest) = (updateFromCurrentModals inEffect x):(totalize' (updateModals inEffect x) rest)

-- |Interpreter state
data IPState = IPState {
    ipModalGroups :: M.Map RS274Group Code
  , ipPosition :: Axes
  , ipLine :: Integer
  } deriving (Eq, Show, Ord)

-- |Create new interpreter state
newState = IPState {
    ipModalGroups = M.empty
  , ipPosition = M.empty
  , ipLine = 0
  }

-- |Evaluate one Code returning new state and remaining GCode
step :: (IPState, GCode) -> (IPState, GCode)
step (is, []) = (is, [])
step (is@IPState{..}, (x@Code{..}:xs)) =
      -- first we update current GCode with missing data
  let new = updateFromCurrentModals ipModalGroups
          $ updateIncompleteFromCurrentModals ipModalGroups x
      -- then we update stored modal groups with updated GCode
      newModals = updateModals ipModalGroups new
      -- update position with new codeAxes
      newPosition = updateAxes ipPosition codeAxes
  in (is { ipModalGroups = newModals
         , ipPosition = newPosition
         , ipLine = ipLine + 1 }, xs)
-- handle empty/comments/other
step (is, (x:xs)) = (is, xs)

-- |Fully evaluate GCode
eval gcode = go initState
  where
    initState = (newState, gcode)
    go x@(_, []) = x
    go x = go . step $ x

-- |Take current motion group modal code and update this motion code
-- with missing coordinates of the stored one
updateFromCurrentModals inEffect x | x `codeInGroup` Motion = do
  case M.lookup Motion inEffect of -- motion group
    Nothing -> x
    (Just e) -> appmod (axes $ appendOnlyAxes (codeAxes x) (codeAxes e)) x
updateFromCurrentModals _ x | otherwise = x

-- |Return True if this code contains only coordinates
incomplete Code{codeCls=Nothing, ..} = True
incomplete Code{codeNum=Nothing, ..} = True
incomplete _ = False

-- |Update incomplete motion Code with the stored one
updateIncompleteFromCurrentModals inEffect x | incomplete x = do
  case M.lookup Motion inEffect of -- motion group
    Nothing -> x
    (Just e) -> appmod (
         (cls $ fromJust $ codeCls e)
      <> (num $ fromJust $ codeNum e)
      <> (axes $ appendOnlyAxes (codeAxes x) (codeAxes e))
      ) x
updateIncompleteFromCurrentModals _ x | otherwise = x

-- |Update axes that aren't defined in target
appendOnlyAxes target from = M.union target missingOnly
  where missingOnly = M.difference from target

-- |Update (replace) target axes with `from` axes
updateAxes target from = M.union from target -- union in this order so `from` axes are preferred

-- |Update `Limits` from this `Code`
updateLimitsCode :: Limits -> Code -> Limits
updateLimitsCode s Code{..} = updateLimits s codeAxes
updateLimitsCode s _ = s

-- |Update `Limits` from `Axes`
updateLimits :: Limits -> Axes -> Limits
updateLimits s = M.foldlWithKey adj s
  where
    adj limits ax val = M.alter (alterfn val) ax limits
    alterfn val (Just (min_c, max_c)) = Just (min min_c val, max max_c val)
    alterfn val Nothing = Just (val, val)
