{-| GCode pretty-printing functions

Utilities for manipulating and filtering 'GCode'

-}
{-# LANGUAGE RecordWildCards #-}
module Data.GCode.Utils where

import Data.GCode.Types
import qualified Data.Map.Strict as M

-- |True if 'Code' is a G-code
isG :: Code -> Bool
isG Code{cls=G, ..} = True
isG _ = False

-- |True if 'Code' is a G{N} code
isGN :: Int -> Code -> Bool
isGN n Code{cls=G, code=x, ..} | x == n = True
isGN _ _ = False

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

-- |True if 'Code' is a M-code
isM :: Code -> Bool
isM Code{cls=M, ..} = True
isM _ = False

-- |True if 'Code' has a coordinate in axis 'a'
hasAxis :: AxisDesignator -> Code -> Bool
hasAxis a Code{..} = M.member a axes
hasAxis a _ = False

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
hasParam p Code{..} = M.member p params
hasParam a _ = False

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
replaceClass newclass Code{..} = Code newclass code sub axes params comment
replaceClass _ x = x

-- |Replace code value of 'Code' (e.g. for chaning G0 to G1)
replaceCode :: Int -> Code -> Code
replaceCode newcode Code{..} = Code cls newcode sub axes params comment
replaceCode _ x = x

-- |Replace axis with 'AxisDesignator' in 'Code' returning new 'Code'
replaceAxis :: AxisDesignator -> Double -> Code -> Code
replaceAxis de val c@Code{..} | hasAxis de c = addReplaceAxis de val c
replaceAxis _ _ c@Code{..} = c

-- |Replace or add axis with 'AxisDesignator' in 'Code' returning new 'Code'
addReplaceAxis :: AxisDesignator -> Double -> Code -> Code
addReplaceAxis de val Code{..} = Code cls code sub (newaxes axes) params comment
  where
    newaxes = M.insert de val
addReplaceAxis _ _ x = x

-- |Replace X axis coordinate
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
replaceParam _ _ c@Code{..} = c

-- |Replace or add parameter with 'ParamDesignator' in 'Code' returning new 'Code'
addReplaceParam :: ParamDesignator -> Double -> Code -> Code
addReplaceParam de val Code{..} = Code cls code sub axes (newparams params) comment
  where
    newparams = M.insert de val
addReplaceParam _ _ x = x

-- |Replace feedrate (F parameter) in 'Code' returning new 'Code'
replaceFeedrate :: Double -> Code -> Code
replaceFeedrate = replaceParam F

-- |Sum of all axis distances of this 'Code'
travel :: Code -> Double
travel Code{cls=G, ..} = M.foldl (+) 0 axes
travel _ = 0
