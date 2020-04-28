module SpecHelper
    ( module Test.Hspec
    , module Data.GCode
    , parseOnly
    , fromString
    , pp
    ) where

import Test.Hspec
import Data.Attoparsec.ByteString (parseOnly)
import Data.GCode
import Data.GCode.Parse
import Data.GCode.Pretty
import Data.String (fromString)

pp = ppGCodeStyle $ defaultStyle { stylePrecision = 2 }
