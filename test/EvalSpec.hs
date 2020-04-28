{-# LANGUAGE OverloadedStrings #-}

module EvalSpec where

import SpecHelper

import Data.GCode.Generate
import Data.GCode.RS274
import Data.GCode.Eval


shouldEvalEqually a b = (ipModalGroups . snd . eval $ a) `shouldBe` (ipModalGroups . snd . eval $ b)
shouldTotalizeEqually a b = (totalize a) `shouldBe` b

spec :: Spec
spec = do
  context "eval" $ do
    it "replaces move commands" $ do
      shouldEvalEqually [ move & xy 6 1 , rapid & xy 0 0 ]
                        [ rapid & xy 0 0 ]

    it "updates moves correctly" $ do
      shouldEvalEqually [ move & xyz 6 1 2 , rapid & xy 0 0 ]
                        [ rapid & xyz 0 0 2]

    it "handles incompletes correctly" $ do
      shouldEvalEqually [ move & xyz 6 1 2 , emptyCode & xy 0 0 ]
                        [ move & xyz 0 0 2]

    it "handles inches" $ do
      shouldEvalEqually [ inches, move & xyz 0 0 1, move & xy 1 1 ]
                        [ move & xyz 25.4 25.4 25.4, inches ]

    it "handles feedrate in inches" $ do
      shouldEvalEqually [ inches, move & xyz 0 0 1, move & xy 1 1 & feed 10 ]
                        [ move & xyz 25.4 25.4 25.4 & feed 254, inches ]

    it "handles relative moves" $ do
      shouldEvalEqually [ relative, move & xyz 1 1 1, move & xy 2 3 ]
                        [ move & xyz 3 4 1, relative ]

    it "handles relative arcs" $ do
      shouldEvalEqually [ arcRelative, move & xyz 1 1 1, arc & xy 10 10 & ij 5 5 ]
                        [ arc & xyz 10 10 1 & ij 6 6, arcRelative ]

    it "handles combined relative/absolute moves" $ do
      shouldEvalEqually [ relative, move & xyz 10 15 19, absolute, move & z 20 ]
                        [ move & xyz 10 15 20 ]

  context "totalize" $ do
    it "totalizes simple moves" $ do
      shouldTotalizeEqually [ move & x 0 , move & y 1, move & z 2 ]
                            [ move & x 0, move & xy 0 1, move & xyz 0 1 2 ]

    it "totalizes incomplete moves" $ do
      shouldTotalizeEqually [ move & x 0 , emptyCode & y 1, emptyCode & z 2 ]
                            [ move & x 0, move & xy 0 1, move & xyz 0 1 2 ]

    it "totalizes mixed moves/rapids" $ do
      shouldTotalizeEqually [ spindleCW,  move & x 0 , rapid & y 1, move & z 2 ]
                            [ spindleCW, move & x 0, rapid & xy 0 1, move & xyz 0 1 2 ]

  context "updateAxes" $ do
      it "updates axes correctly" $ do
        let from = codeAxes $ move & y 1 & z 2
            to = codeAxes $ move & x 0
        (updateAxes from to) `shouldBe` (codeAxes $ move & xyz 0 1 2)
