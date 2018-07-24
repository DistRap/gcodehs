{-| GCode types

This module exports types for constructing 'Code' values

-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
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
    , codecls
    , axis
    , axis'
    , param
    , param'
    , CodeMod
    , cls
    , num
    , sub
    , axes
    , params
    , comment
    , appmod
    , eval
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
import Control.Monad.State.Strict
import Control.Applicative

import Data.Aeson
import GHC.Generics
import qualified Data.Map.Strict as M



-- | Code class
data Class =
    G   -- ^ G-code
  | M   -- ^ M-code
  | T   -- ^ T-code (temperature)
  | StP -- ^ Stand-alone P-code
  | StF -- ^ Stand-alone F-code
  | StS -- ^ Stand-alone S-code
  deriving (Generic, Show, Enum, Eq, Ord)

-- | Axis letter
data AxisDesignator =
    X -- ^ X-axis
  | Y -- ^ Y-axis
  | Z -- ^ Z-axis
  | A -- ^ A-axis
  | B -- ^ B-axis
  | C -- ^ C-axis
  | E -- ^ Extruder axis
  | L
  deriving (Generic, Show, Enum, Eq, Ord)

-- | Param letter
data ParamDesignator =
    S -- ^ S parameter - usually spindle RPM
  | P -- ^ P parameter
  | F -- ^ F parameter - usually feedrate
  | R -- ^ R parameter
  deriving (Generic, Show, Enum, Eq, Ord)

-- |Convert 'Char' representation of a code to its 'Class'
codecls :: Char -> Class
codecls 'G' = G
codecls 'M' = M
codecls 'T' = T
codecls 'P' = StP
codecls 'F' = StF
codecls 'S' = StS

-- |Convert 'Char' representation of an axis to its 'AxisDesignator'
axis :: Char -> AxisDesignator
axis 'X' = X
axis 'Y' = Y
axis 'Z' = Z
axis 'A' = A
axis 'B' = B
axis 'C' = C
axis 'E' = E
axis 'L' = L

-- |Convert 'Char' representation of a param to its 'ParamDesignator'
param :: Char -> ParamDesignator
param 'S' = S
param 'P' = P
param 'F' = F
param 'R' = R

-- | Map of 'AxisDesignator' to 'Double'
type Axes = M.Map AxisDesignator Double

type Limits = M.Map AxisDesignator (Double, Double)

-- | Map of 'ParamDesignator' to 'Double'
type Params = M.Map ParamDesignator Double

type ParamLimits = M.Map ParamDesignator (Double, Double)

-- | List of 'Code's
type GCode = [Code]

data Code =
    Code {
        codeCls :: Maybe Class            -- ^ Code 'Class' (M in M5)
      , codeNum :: Maybe Int             -- ^ Code value (81 in G81)
      , codeSub :: Maybe Int              -- ^ Code subcode (1 in G92.1)
      , codeAxes :: Axes            -- ^ Code 'Axes'
      , codeParams :: Params        -- ^ Code 'Params'
      , codeComment :: B.ByteString -- ^ Comment following this Code
    }
  | Comment B.ByteString        -- ^ Standalone comment
  | Empty                       -- ^ Empty lines
  | Other B.ByteString          -- ^ Parser unhandled lines
  deriving (Generic, Show, Eq, Ord)


instance ToJSON ParamDesignator
instance FromJSON ParamDesignator
instance ToJSON Class
instance ToJSON AxisDesignator
instance FromJSON AxisDesignator
instance FromJSON Class
instance ToJSON Code
instance FromJSON Code

instance ToJSON v => ToJSON (M.Map ParamDesignator v) where
  toJSON = toJSON . M.mapKeys show

instance FromJSON v => FromJSON (M.Map ParamDesignator v) where
  parseJSON v = convert <$> parseJSON v
      where convert :: M.Map T.Text v -> M.Map ParamDesignator v
            convert =  M.fromList . map (\(x,y) -> (param $ T.head x,y)) . M.toList

instance ToJSON v => ToJSON (M.Map AxisDesignator v) where
  toJSON = toJSON . M.mapKeys show

instance FromJSON v => FromJSON (M.Map AxisDesignator v) where
  parseJSON v = convert <$> parseJSON v
      where convert :: M.Map T.Text v -> M.Map AxisDesignator v
            convert =  M.fromList . map (\(x,y) -> (axis $ T.head x,y)) . M.toList

instance ToJSON B.ByteString where
  toJSON = String . TE.decodeUtf8
  {-# INLINE toJSON #-}

instance FromJSON B.ByteString where
  parseJSON = withText "ByteString" $ pure . TE.encodeUtf8
  {-# INLINE parseJSON #-}


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

axis' :: AxisDesignator -> Double -> CodeMod
axis' des val = CodeMod $ \c -> c { codeAxes = M.insert des val $ codeAxes c }

params :: Params -> CodeMod
params x = CodeMod $ \c -> c { codeParams = x}

param' :: ParamDesignator -> Double -> CodeMod
param' des val = CodeMod $ \c -> c { codeParams = M.insert des val $ codeParams c }

comment :: B.ByteString -> CodeMod
comment x = CodeMod $ \c -> c { codeComment = x}

appmod :: CodeMod -> Code -> Code
appmod m c = applyCodeMod m c

--data Sim = Empty
--  | Line Axes Axes
--  deriving (Show, Eq)

--eval c1 c2 = Line (codeAxes c1) (codeAxes c2)
eval = undefined

emptyCode = Code Nothing Nothing Nothing M.empty M.empty ""



data Style =
  Style {
      stylePrecision :: Int
    , styleColorful :: Bool
  } deriving (Show)

defaultPrec :: Int
defaultPrec = 6

defaultStyle = Style defaultPrec False
