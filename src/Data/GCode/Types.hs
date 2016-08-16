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
    , Code(..)
    , GCode
    , codecls
    , axis
    , param
    ) where

import qualified Data.ByteString      as B
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as TE

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
  | F -- ^ S parameter - usually feedrate
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

-- | Map of 'AxisDesignator' to 'Double'
type Axes = M.Map AxisDesignator Double

-- | Map of 'ParamDesignator' to 'Double'
type Params = M.Map ParamDesignator Double

-- | List of 'Code's
type GCode = [Code]

data Code =
    Code {
        cls :: Class            -- ^ Code 'Class' (M in M5)
      , code :: Int             -- ^ Code value (81 in G81)
      , sub :: Int              -- ^ Code subcode (1 in G92.1)
      , axes :: Axes            -- ^ Code 'Axes'
      , params :: Params        -- ^ Code 'Params'
      , comment :: B.ByteString -- ^ Comment following this Code
    }
  | Comment B.ByteString        -- ^ Standalone comment
  | Other B.ByteString          -- ^ Parser unhandled lines (should contain blank lines only)
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
