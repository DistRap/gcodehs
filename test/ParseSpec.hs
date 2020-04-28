{-# LANGUAGE OverloadedStrings #-}

module ParseSpec where

import SpecHelper

import qualified Data.ByteString.Char8 as BSC

spec :: Spec
spec = do
  let roundTrip n = (fmap pp $ parseOnly parseGCode n) `shouldBe` (Right (BSC.unpack n))
  it "roundtrips" $ do
    roundTrip "M3\n"

    roundTrip "M117 L180.00 S8640.00\n"

    roundTrip "G0 X1.00 Y2.00 Z3.33\nM114\n"
