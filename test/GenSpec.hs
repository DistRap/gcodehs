{-# LANGUAGE OverloadedStrings #-}

module GenSpec where

import SpecHelper

import Data.GCode.Generate
import Data.GCode.RS274

spec :: Spec
spec = do
  it "moves" $ do
    pp [ move & xyz 1 2 3 ] `shouldBe` "G1 X1.00 Y2.00 Z3.00\n"

  it "rapids" $ do
    pp [ rapid & xy 13.37 48 ] `shouldBe` "G0 X13.37 Y48.00\n"

  it "mcodes" $ do
    pp [ m <#> 5, m <#> 9, m <#> 2 ] `shouldBe` "M5\nM9\nM2\n"

  it "params" $ do
    pp [ g <#> 4 & param P 10 ] `shouldBe` "G4 P10.00\n"

  it "subcodes" $ do
    pp [ g <#> 5 & sub 2 ] `shouldBe` "G5.2\n"
