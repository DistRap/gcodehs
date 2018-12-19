{-| GCode types

This module exports types for constructing 'Code' values

-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.GCode.Types (
      Class(..)
    , AxisDesignator(..)
    , ParamDesignator(..)
    , Axes
    , Params
    , Limits
    , ParamLimits
    , Code(..)
    , GCode
    , toCodeClass
    , toAxis
    , toParam
    , CodeMod(..)
    , cls
    , axis
    , param
    , num
    , sub
    , axes
    , params
    , comment
    , appmod
    , emptyCode
    , defaultPrec
    , Style(..)
    , defaultStyle
    ) where

import qualified Data.ByteString      as B
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as TE

--import qualified Foldable as F
import Data.Semigroup hiding (option)
import Control.Applicative

import qualified Data.Map.Strict as M

-- | Code class
data Class =
    G   -- ^ G-code
  | M   -- ^ M-code
  | T   -- ^ T-code (select tool)
  | StP -- ^ Stand-alone P-code
  | StF -- ^ Stand-alone F-code
  | StS -- ^ Stand-alone S-code
  deriving (Show, Enum, Eq, Ord)

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

-- | Param letter
data ParamDesignator =
    S -- ^ S parameter - usually spindle RPM
  | P -- ^ P parameter
  | F -- ^ F parameter - usually feedrate
  | R -- ^ R parameter
  deriving (Show, Enum, Eq, Ord)

-- |Convert 'Char' representation of a code to its 'Class'
toCodeClass :: Char -> Class
toCodeClass 'G' = G
toCodeClass 'M' = M
toCodeClass 'T' = T
toCodeClass 'P' = StP
toCodeClass 'F' = StF
toCodeClass 'S' = StS

-- |Convert 'Char' representation of an axis to its 'AxisDesignator'
toAxis :: Char -> AxisDesignator
toAxis 'X' = X
toAxis 'Y' = Y
toAxis 'Z' = Z
toAxis 'A' = A
toAxis 'B' = B
toAxis 'C' = C
toAxis 'U' = U
toAxis 'V' = V
toAxis 'W' = W
toAxis 'E' = E
toAxis 'L' = L

-- |Convert 'Char' representation of a param to its 'ParamDesignator'
toParam :: Char -> ParamDesignator
toParam 'S' = S
toParam 'P' = P
toParam 'F' = F
toParam 'R' = R

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

newtype CodeMod = CodeMod
  { applyCodeMod :: Code -> Code }

instance Monoid CodeMod where
  mempty = CodeMod id
  mappend = (<>)

instance Semigroup CodeMod where
  m1 <> m2 = CodeMod $ applyCodeMod m1 . applyCodeMod m2

cls :: Class -> CodeMod
cls x = CodeMod $ \c -> c { codeCls = Just x}

num :: Int -> CodeMod
num x = CodeMod $ \c -> c { codeNum = Just x}

sub :: Int -> CodeMod
sub x = CodeMod $ \c -> c { codeSub = Just x}

axes :: Axes -> CodeMod
axes x = CodeMod $ \c -> c { codeAxes = x}

axis :: AxisDesignator -> Double -> CodeMod
axis des val = CodeMod $ \c -> c { codeAxes = M.insert des val $ codeAxes c }

params :: Params -> CodeMod
params x = CodeMod $ \c -> c { codeParams = x}

param :: ParamDesignator -> Double -> CodeMod
param des val = CodeMod $ \c -> c { codeParams = M.insert des val $ codeParams c }

comment :: B.ByteString -> CodeMod
comment x = CodeMod $ \c -> c { codeComment = x}

appmod :: CodeMod -> Code -> Code
appmod m c = applyCodeMod m c

emptyCode = Code Nothing Nothing Nothing M.empty M.empty ""

data Style =
  Style {
      stylePrecision :: Int
    , styleColorful :: Bool
  } deriving (Show)

defaultPrec :: Int
defaultPrec = 6

defaultStyle = Style defaultPrec False
