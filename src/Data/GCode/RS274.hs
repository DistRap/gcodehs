{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Data.GCode.RS274 where

import Data.GCode.TH
import Data.GCode.Types
import Data.GCode.RS274.Types

import Data.Maybe (fromJust)
import qualified Data.Map.Strict as M

$(genShortcuts ''RS274Name)

namesToCodes = M.fromList . map (\x -> (defName x, toCode x)) $ allCodes
codesToNames = M.fromList . map (\x -> (toCode x, defName x)) $ allCodes
codesToGroups = M.fromList . map (\x -> (toCode x, defGroup x)) $ allCodes

codesToDefs = M.fromList . map (\x -> (toCode x, x)) $ allCodes

codeIsRS274 code name = (M.lookup (decimate code) codesToNames) == (Just name)
codeInGroup code group = (fmap defGroup $ M.lookup (decimate code) codesToDefs) == (Just group)

explain code@Code{} = case M.lookup (decimate code) codesToDefs of
  Nothing -> ""
  Just def -> defHelp def
explain x = ""

-- only to be used by TH
codeFromName :: RS274Name -> Code
codeFromName n = fromJust $ M.lookup n namesToCodes

-- unused
eqClassNumSub :: Code -> Code -> Bool
eqClassNumSub a b = (decimate a) == (decimate b)

-- strip this code of its axes/parameters/comments
-- copy just class, code number and subcode
decimate :: Code -> Code
decimate x@Code{} | codeCls x `elem` (map Just [T, FStandalone, PStandalone, SStandalone]) = copyClass x emptyCode
decimate x@Code{} = copyClassNumSub x emptyCode
decimate x = x


copyClassNumSub from to = to { codeCls = codeCls from
                             , codeNum = codeNum from
                             , codeSub = codeSub from }

copyClass from to = to { codeCls = codeCls from }
