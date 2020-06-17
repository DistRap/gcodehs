{-| GCode pretty-printing functions

Utilities for manipulating and filtering 'GCode'

-}
{-# LANGUAGE RecordWildCards #-}
module Data.GCode.Utils where

import Data.Maybe
import Data.Monoid

import Data.GCode.Types
import Data.GCode.RS274 (isMove, isRapid)
import qualified Data.Map.Strict as M

-- | True if 'Code' is a G-code
isG :: Code -> Bool
isG Code{codeCls=(Just G), ..} = True
isG _ = False

-- | True if 'Code' is a M-code
isM :: Code -> Bool
isM Code{codeCls=(Just M), ..} = True
isM _ = False

-- | True if 'Code' is a G{N} code
isGN :: Int -> Code -> Bool
isGN n Code{codeCls=(Just G), codeNum=(Just x), ..} = x == n
isGN _ _ = False

-- | True if 'Code' has a coordinate in axis 'a'
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

-- | True if 'Code' contains 'X' axis
hasX :: Code -> Bool
hasX = hasAxis X

-- | True if 'Code' contains 'Y' axis
hasY :: Code -> Bool
hasY = hasAxis Y

-- | True if 'Code' contains 'Z' axis
hasZ :: Code -> Bool
hasZ = hasAxis Z

-- | True if 'Code' contains 'E' axis
hasE :: Code -> Bool
hasE = hasAxis E

-- | True if 'Code' contains parameter with 'ParamDesignator'
hasParam :: ParamDesignator -> Code -> Bool
hasParam p Code{..} = M.member p codeParams
hasParam a _ = False

-- | Get parameter if defined
getParam :: ParamDesignator -> Code -> Maybe Double
getParam p Code{..} = M.lookup p codeParams

-- | True if 'Code' contains feedrate parameter (e.g. G0 F3000)
hasFeedrate :: Code -> Bool
hasFeedrate = hasParam F

-- | Filter G-codes
gcodes :: [Code] -> [Code]
gcodes = filter isG

-- | Filter M-codes
mcodes :: [Code] -> [Code]
mcodes = filter isM

-- | Filter rapid moves
rapids :: [Code] -> [Code]
rapids = filter isRapid

-- | Filter moves
moves :: [Code] -> [Code]
moves  = filter isMove

-- | Replace 'Class' of 'Code' (e.g. for chaning G0 to M0)
replaceClass :: Class -> Code -> Code
replaceClass newclass c = cls newclass c

-- | Replace code value of 'Code' (e.g. for chaning G0 to G1)
replaceCode :: Int -> Code -> Code
replaceCode newcode c = num newcode c

-- | Replace axis with 'AxisDesignator' in 'Code' returning new 'Code'
replaceAxis :: AxisDesignator -> Double -> Code -> Code
replaceAxis de val c@Code{..} | hasAxis de c = addReplaceAxis de val c
replaceAxis _ _ c = c

-- | Apply function to axis specified by 'AxisDesignator'
modifyAxis :: AxisDesignator -> (Double -> Double) -> Code -> Code
modifyAxis de f c@Code{..} | hasAxis de c = addReplaceAxis de (f $ fromJust $ getAxis de c) c
modifyAxis _ _ c = c

-- | Apply function to axes specified by '[AxisDesignator]'
modifyAxes :: [AxisDesignator] -> (Double -> Double) -> Code -> Code
modifyAxes axes f c = foldl (\c1  ax -> modifyAxis ax f c1) c axes

-- | Test if Code has X and Y axes
hasXY c = hasAxis X c && hasAxis Y c

-- | Apply function to X and Y axes
modifyXY :: (Double -> Double -> (Double, Double)) -> Code -> Code
modifyXY f c | hasXY c =
  let x = fromJust $ getAxis X c
      y = fromJust $ getAxis Y c
      (nx, ny) = f x y
  in c & axis X nx & axis Y ny
modifyXY _ c = c

-- | Replace or add axis with 'AxisDesignator' in 'Code' returning new 'Code'
addReplaceAxis :: AxisDesignator -> Double -> Code -> Code
addReplaceAxis de val c@Code{..} = c & (axes $ newaxes $ codeAxes)
  where
    newaxes = M.insert de val
addReplaceAxis _ _ x = x

-- | Replace X axis coordnate
replaceX :: Double -> Code -> Code
replaceX = replaceAxis X

-- | Replace Y axis coordinate
replaceY :: Double -> Code -> Code
replaceY = replaceAxis Y

-- | Replace Z axis coordinate
replaceZ :: Double -> Code -> Code
replaceZ = replaceAxis Z

-- | Replace E axis coordinate
replaceE :: Double -> Code -> Code
replaceE = replaceAxis E

-- | Replace or add X axis coordinate
addReplaceX :: Double -> Code -> Code
addReplaceX = addReplaceAxis X

-- | Replace or add Y axis coordinate
addReplaceY :: Double -> Code -> Code
addReplaceY = addReplaceAxis Y

-- | Replace or add Z axis coordinate
addReplaceZ :: Double -> Code -> Code
addReplaceZ = addReplaceAxis Z

-- | Replace or add E axis coordinate
addReplaceE :: Double -> Code -> Code
addReplaceE = addReplaceAxis E

-- | Replace parameter with 'ParamDesignator' in 'Code' returning new 'Code'
replaceParam :: ParamDesignator -> Double -> Code -> Code
replaceParam de val c@Code{..} | hasParam de c = addReplaceParam de val c
replaceParam _ _ c = c

-- | Apply function to parameter with 'ParamDesignator'
modifyParam :: ParamDesignator -> (Double -> Double) -> Code -> Code
modifyParam de f c@Code{..} | hasParam de c = addReplaceParam de (f $ fromJust $ getParam de c) c
modifyParam _ _ c = c

-- | Apply function to parameters specified by '[ParamDesignator]'
modifyParams :: [ParamDesignator] -> (Double -> Double) -> Code -> Code
modifyParams params f c = foldl (\c1 ax -> modifyParam ax f c1) c params

-- | Apply function to parameters specified by '[ParamDesignator]'
--
-- Function gets 'ParameterDesignator' passed as its first argument
modifyParamsWithKey :: [ParamDesignator] -> (ParamDesignator -> Double -> Double) -> Code -> Code
modifyParamsWithKey params f c = foldl (\c1 ax -> modifyParam ax (f ax) c1) c params

-- | Replace or add parameter with 'ParamDesignator' in 'Code' returning new 'Code'
addReplaceParam :: ParamDesignator -> Double -> Code -> Code
addReplaceParam de val c@Code{..} = c & (params $ newparams $ codeParams)
  where
    newparams = M.insert de val
addReplaceParam _ _ x = x

-- | Replace feedrate (F parameter) in 'Code' returning new 'Code'
replaceFeedrate :: Double -> Code -> Code
replaceFeedrate = replaceParam F

-- | Apply function to feedrate
modifyFeedrate :: (Double -> Double) -> Code -> Code
modifyFeedrate = modifyParam F

-- | Sum of all axis distances of this 'Code'
travelDistance :: Code -> Double
travelDistance Code{codeCls=(Just G), ..} = M.foldl (+) 0 codeAxes
travelDistance _ = 0

-- | Round `x` with specified precision
roundprec n x = (fromInteger $ round $ x * (10^n)) / (10.0^^n)
