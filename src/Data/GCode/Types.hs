{-| GCode types

This module exports types for constructing 'Code' values

-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.GCode.Types (
      Class(..)
    , AxisDesignator(..)
    , ParamDesignator(..)
    , allClasses
    , allAxisDesignators
    , allParamDesignators
    , asChars
    , Axes
    , Params
    , Limits
    , ParamLimits
    , Code(..)
    , GCode
    , toCodeClass
    , toAxis
    , toParam
    , (&)
    , cls
    , axis
    , param
    , num
    , sub
    , axes
    , params
    , comment
    , emptyCode
    , defaultPrec
    , Style(..)
    , defaultStyle
    ) where

import Data.Char                      (toUpper)
import qualified Data.ByteString      as B
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as TE

import qualified Data.Map.Strict as M

-- | Code class
data Class =
    G           -- ^ G-code
  | M           -- ^ M-code
  | T           -- ^ T-code (select tool)
  | PStandalone -- ^ Stand-alone P-code
  | FStandalone -- ^ Stand-alone F-code
  | SStandalone -- ^ Stand-alone S-code
  deriving (Show, Enum, Eq, Ord)

allClasses = [G, M, T, PStandalone, FStandalone, SStandalone]

-- | Axis letter
data AxisDesignator =
    X -- ^ X-axis
  | Y -- ^ Y-axis
  | Z -- ^ Z-axis
  | A -- ^ A-axis
  | B -- ^ B-axis
  | C -- ^ C-axis
  | U -- ^ U-axis
  | V -- ^ V-axis
  | W -- ^ W-axis
  | E -- ^ Extruder axis
  | L
  deriving (Show, Enum, Eq, Ord)

allAxisDesignators = [X, Y, Z, A, B, C, U, V, W, E, L]

-- | Param letter
data ParamDesignator =
    S -- ^ S parameter - usually spindle RPM
  | P -- ^ P parameter
  | F -- ^ F parameter - usually feedrate
  | H -- ^ H paramater - used by tool length offset
  | R -- ^ R parameter
  | I -- ^ X offset for arcs
  | J -- ^ Y offset for arcs
  | K -- ^ Z offset for arcs
  deriving (Show, Enum, Eq, Ord)

allParamDesignators = [S, P, F, R, I, J, K]

asChars types = map ((!! 0) . show) types
fromChar c types = M.lookup (toUpper c) $ M.fromList (zip (asChars types) types)

-- |Convert 'Char' representation of a code to its 'Class'
toCodeClass :: Char -> Maybe Class
toCodeClass c = fromChar c allClasses

-- |Convert 'Char' representation of an axis to its 'AxisDesignator'
toAxis :: Char -> Maybe AxisDesignator
toAxis c = fromChar c allAxisDesignators

-- |Convert 'Char' representation of a param to its 'ParamDesignator'
toParam :: Char -> Maybe ParamDesignator
toParam c = fromChar c allParamDesignators

-- | Map of 'AxisDesignator' to 'Double'
type Axes = M.Map AxisDesignator Double

-- | Map of 'AxisDesignator' to pair of 'Double's indicating lower and upper limits of travel
type Limits = M.Map AxisDesignator (Double, Double)

-- | Map of 'ParamDesignator' to 'Double'
type Params = M.Map ParamDesignator Double

-- | Map of 'ParamDesignator' to pair of 'Double's indicating lower and upper limits of this parameter
type ParamLimits = M.Map ParamDesignator (Double, Double)

-- | List of 'Code's
type GCode = [Code]

data Code =
    Code {
        codeCls :: Maybe Class      -- ^ Code 'Class' (M in M5)
      , codeNum :: Maybe Int        -- ^ Code value (81 in G81)
      , codeSub :: Maybe Int        -- ^ Code subcode (1 in G92.1)
      , codeAxes :: Axes            -- ^ Code 'Axes'
      , codeParams :: Params        -- ^ Code 'Params'
      , codeComment :: B.ByteString -- ^ Comment following this Code
    }
  | Comment B.ByteString        -- ^ Standalone comment
  | Empty                       -- ^ Empty lines
  | Other B.ByteString          -- ^ Parser unhandled lines
  deriving (Show, Eq, Ord)


-- endofunctors for manipulating `Code`
cls :: Class -> Code -> Code
cls x c = c { codeCls = Just x}

num :: Int -> Code -> Code
num x c = c { codeNum = Just x}

sub :: Int -> Code -> Code
sub x c = c { codeSub = Just x}

axes :: Axes -> Code -> Code
axes x c = c { codeAxes = x}

axis :: AxisDesignator -> Double -> Code -> Code
axis des val c = c { codeAxes = M.insert des val $ codeAxes c }

params :: Params -> Code -> Code
params x c = c { codeParams = x}

param :: ParamDesignator -> Double -> Code -> Code
param des val c = c { codeParams = M.insert des val $ codeParams c }

comment :: B.ByteString -> Code -> Code
comment x c = c { codeComment = x}

-- code & num 10 & comment "& example"
(&) = flip ($)

emptyCode = Code {
    codeCls = Nothing
  , codeNum = Nothing
  , codeSub = Nothing
  , codeAxes = M.empty
  , codeParams = M.empty
  , codeComment = ""
  }


data Style =
  Style {
      stylePrecision :: Int
    , styleColorful :: Bool
  } deriving (Show)

defaultPrec :: Int
defaultPrec = 6

defaultStyle = Style defaultPrec False
