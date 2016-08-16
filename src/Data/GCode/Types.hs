{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

data Class = G | M | T | StP | StF | StS
  deriving (Generic, Show, Enum, Eq, Ord)
data AxisDesignator = X | Y | Z | A | B | C | E | L
  deriving (Generic, Show, Enum, Eq, Ord)
data ParamDesignator = S | P | F
  deriving (Generic, Show, Enum, Eq, Ord)

codecls 'G' = G
codecls 'M' = M
codecls 'T' = T
codecls 'P' = StP
codecls 'F' = StF
codecls 'S' = StS

axis 'X' = X
axis 'Y' = Y
axis 'Z' = Z
axis 'A' = A
axis 'B' = B
axis 'C' = C
axis 'E' = E
axis 'L' = L

param 'S' = S
param 'P' = P
param 'F' = F

type Axes = M.Map AxisDesignator Double
type Params = M.Map ParamDesignator Double

type GCode = [Code]

data Code =
    Code {
        cls :: Class
      , code :: Int
      , sub :: Int
      , axes :: Axes
      , params :: Params
      , comment :: B.ByteString
    }
  | Comment B.ByteString
  | Other B.ByteString
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
  toJSON = toJSON . M.mapKeys (show)

instance FromJSON v => FromJSON (M.Map ParamDesignator v) where
  parseJSON v = convert <$> parseJSON v
      where convert :: M.Map T.Text v -> M.Map ParamDesignator v
            convert =  M.fromList . map (\(x,y) -> (param $ T.head x,y)) . M.toList

instance ToJSON v => ToJSON (M.Map AxisDesignator v) where
  toJSON = toJSON . M.mapKeys (show)

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
