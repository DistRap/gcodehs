{-| Examples of GCode generation

-}
module Data.GCode.Generate.Examples where

import Data.GCode
import Data.GCode.RS274
import Data.GCode.Generate

allExamples = [
    ("encoder_wheel_drilling", fst $ encoderWheel)
  , ("encoder_wheel_milling",  snd $ encoderWheel)
  , ("rectangle10x20",         rectangle10mm20mm)
  ]

preamble = [
    unitsPerMinute
  , absolute
  , millimeters
  , g <#> 3000
  , s <#> 12000
  , spindleCW
  , dwell & param P 10
  ]

postamble = [
    spindleStop
  , coolantStop
  , programEnd
  ]

returnZ = 1
safeZ   = 2
workZ   = (-2)
rapidFeedrate = 250
downFeedrate = 150

up   = rapid & z safeZ & feed rapidFeedrate
down = move  & z workZ & feed downFeedrate

program code = preamble ++ code ++ postamble

rectangle10mm20mm :: GCode
rectangle10mm20mm = program $ map (uncurry movexy) (rectangle 10 20)

encoderWheel :: (GCode, GCode)
encoderWheel =
    let encoderRadius = 50
        encoderSteps  = 100
        drillRadius   = 0.6
        endmillRadius = 3.175
        innerOffset   = 4
        encoderRadiusInner = encoderRadius - (2*drillRadius) - innerOffset

        -- drilling
        drillPoints      = circle encoderRadius encoderSteps
        drillPointsInner = circle' (2*pi/360 * (360 / 200) * 1.5) encoderRadiusInner encoderSteps

        drill x y = drillingCycle & xyz x y workZ & feed 250 & param R returnZ
        drillBlocks = [ map (uncurry drill) drillPoints
                      , map (uncurry drill) drillPointsInner ]
        -- milling
        anchor = circle (5.3 - endmillRadius) 360
        anchorPositions = circle (30) 4

        anchors = travelCat up down $ map (\(xc, yc) ->
                    map (\(x,y) -> movexy (xc + x) (yc + y)) anchor)
                    anchorPositions

        encInner = circle (10.4 - endmillRadius) 360
        encOuter = circle (encoderRadius + innerOffset + endmillRadius) 3600

        cut = map (uncurry movexy)
        inner = cut encInner
        outer = cut encOuter

    in ( program $ travelCatDrill up drillBlocks
       , program $ travelCat up down [inner, anchors, outer]
       )
