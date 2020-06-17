module Data.GCode.Pipes.Transform where
{--

These are remnants from refactoring Main with various degree of usability.

Do not rely on these as they might get removed (or ideally improved) in future versions.

--}

import Data.GCode
import Data.GCode.Generate (rot)

import Pipes
import qualified Pipes.Prelude as P

translateXY :: Functor m => Double -> Double -> Pipe Code Code m r
translateXY xtrans ytrans = P.map (modifyXY (\x y -> (x + xtrans, y + ytrans)))

translateZ :: Functor m => Double -> Pipe Code Code m r
translateZ ztrans = P.map (modifyAxis Z (+ztrans))

rotate :: Functor m => Double -> Pipe Code Code m r
rotate angle = P.map (modifyXY (rot (angle*pi/180)))

scaleFeedrate :: Functor m => Double -> Pipe Code Code m r
scaleFeedrate factor = P.map (modifyFeedrate (*factor))

scaleXY :: Functor m => Double -> Double -> Pipe Code Code m r
scaleXY xsc ysc = P.map (modifyXY (\x y -> (x*xsc, y*ysc)))
