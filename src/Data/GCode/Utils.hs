{-# LANGUAGE RecordWildCards #-}
module Data.GCode.Utils where

import Data.GCode.Types
import qualified Data.Map.Strict as M

travel Code{cls=G, ..} = M.foldl (+) 0 axes
travel _ = 0

isG Code{cls=G, ..} = True
isG _ = False

isGN n Code{cls=G, code=x, ..} | x == n = True
isGN _ _ = False

isG0 = isGN 0
isRapid = isG0

isG1 = isGN 1
isMove = isG1

isM Code{cls=M, ..} = True
isM _ = False

hasAxis a Code{..} = M.member a axes
hasAxis a _ = False

hasX = hasAxis X
hasY = hasAxis Y
hasZ = hasAxis Z
hasE = hasAxis E

hasParam p Code{..} = M.member p params
hasParam a _ = False

hasFeedrate = hasParam F

gcodes = filter isG
mcodes = filter isM
rapids = filter isRapid
moves  = filter isMove

replaceClass newclass Code{..} = Code newclass code sub axes params comment
replaceClass _ x = x
replaceCode newcode Code{..} = Code cls newcode sub axes params comment
replaceCode _ x = x

replaceAxis de val c@Code{..} | hasAxis de c = addReplaceAxis de val c
replaceAxis _ _ c@Code{..} = c

addReplaceAxis de val Code{..} = Code cls code sub (newaxes axes) params comment
  where
    newaxes = M.insert de val
addReplaceAxis _ _ x = x

replaceX = replaceAxis X
replaceY = replaceAxis Y
replaceZ = replaceAxis Z
replaceE = replaceAxis E

addReplaceX = addReplaceAxis X
addReplaceY = addReplaceAxis Y
addReplaceZ = addReplaceAxis Z
addReplaceE = addReplaceAxis E

replaceParam de val c@Code{..} | hasParam de c = addReplaceParam de val c
replaceParam _ _ c@Code{..} = c

addReplaceParam de val Code{..} = Code cls code sub axes (newparams params) comment
  where
    newparams = M.insert de val
addReplaceParam _ _ x = x

replaceFeedrate = replaceParam F
