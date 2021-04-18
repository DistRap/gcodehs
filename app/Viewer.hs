{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

import Control.Monad

import Graphics.GPipe hiding (Line, (^-^))
--import Graphics.UI.GLFW (WindowHint(..))
import Graphics.GPipe.Context.GLFW as GLFW

import GHC.Float (double2Float)

import Data.GCode
import Data.GCode.Line (Line (..), LineType (..))
import qualified Data.GCode.Pipes

import qualified Data.Map
import qualified Data.Maybe
import qualified Data.Set

import Pipes ((>->))
import qualified Pipes.Prelude

loadFile :: FilePath -> IO (Maybe ([Line], Limits))
loadFile f = do
  res <- pipeToList' f
    $ (fmap Left evalCanonStateP' )
      >-> (fmap Right trackWorkLimits)
      >-> (fmap Left (Pipes.Prelude.map fst >-> evalCanonLinesP'))

  case res of
    (ls, Right (Right limits)) -> return $ pure (ls, limits)
    _                             -> return Nothing

demoLines :: IO [Line]
demoLines =
  Data.GCode.Pipes.gcodeToLines
    "./CubeInCube.gcode"

remap :: Line -> (V3 Float, V3 Float)
remap = toV3 . boring
  where
    -- only XYZ coords
    boring (Line typ from to) = Line typ (filterXYZ from) (filterXYZ to)

    filterXYZ = flip Data.Map.restrictKeys (Data.Set.fromList [X, Y, Z])

    -- this is safe since we always have all axes and only filter boring ones
    xyzToV3 as =
      let [x, y, z] = map (\a -> double2Float . Data.Maybe.fromJust $ Data.Map.lookup a as) [X, Y, Z]
      in V3 x y z

    toV3 (Line typ _from to) = (xyzToV3 to, case typ of
      LineDrawing  -> V3 1 0 0
      LineTraverse -> V3 0 1 0
      LineJump     -> V3 0 0 1)

main :: IO ()
main = do
  rawLines <- demoLines
  --error $ show $ map remap lines
  runContextT GLFW.defaultHandleConfig $ do
    lineBuf :: Buffer os (B3 Float, B3 Float) <- newBuffer $ length rawLines
    uniform :: Buffer os (Uniform (V4 (B4 Float), V3 (B3 Float))) <- newBuffer 1

    writeBuffer lineBuf 0 $ map remap rawLines

    win <- newWindow
      (WindowFormatColorDepth SRGB8 Depth16)
      ((GLFW.defaultWindowConfig "GPipe gcodehs viewer")
        { GLFW.configHints = [ WindowHint'Samples $ Just 16 ] }
      )

    shader <- compileShader $ do
      primitiveStream <- toPrimitiveStream id

      (modelViewProj, normMat) <- getUniform (const (uniform, 0))
      let
          projected = proj modelViewProj normMat <$> primitiveStream

      fragmentStream <- rasterize
        (const
          (FrontAndBack
          , PolygonFill
          , ViewPort (V2 0 0) (V2 (1920 `div` 1) 1080)
          , DepthRange 0 1)
        )
        projected

      drawWindowColor (const (win, ContextColorOption NoBlending (V3 True True True)))
        fragmentStream

    loop win shader lineBuf uniform 0 0.02

proj
  :: (Functor m, Num a)
  => m (V4 a)
  -> p
  -> (V3 a, b)
  -> (m a, b)
proj modelViewProj _normMat (V3 px py pz, color) =
  (modelViewProj !* V4 px py pz 1, color)

loop
  :: forall os
   .  Window os RGBFloat Depth
  -> (PrimitiveArray Lines (B3 Float, B3 Float) -> Render os ())
  -> Buffer os (B3 Float, B3 Float)
  -> Buffer os (Uniform (V4 (B4 Float), V3 (B3 Float)))
  -> Float
  -> Float
  -> ContextT GLFW.Handle os IO ()
loop win shader vertexBuffer uniform angleRot objectScale = do
  (V2 w h) <- getFrameBufferSize win
  let
      modelRot = fromQuaternion (axisAngle (V3 0 0 1) angleRot)
      modelMat =
            mkTransformationMat modelRot (pure 0)
        !*! mkScaleTransform objectScale

      projMat = perspective (pi/2) (fromIntegral w / fromIntegral h) 1 100
      --cameraMatrix :: Float -> L.M44 Float
      --cameraMatrix t = lookAt eye (V3 0 0 0) (V3 0 1 0)
      --  where eye = L.rotate (axisAngle (V3 0 1 0) (t / 10)) (V3 1 1 1)
      --viewMat = cameraMatrix 0
      viewMat = mkTransformationMat identity (- V3 0 0 5)
      viewProjMat = projMat !*! viewMat !*! modelMat
  writeBuffer uniform 0 [(viewProjMat, identity)]

  render $ do
    clearWindowColor win (V3 0 0 0)
    vertexArray <- newVertexArray vertexBuffer
    let primitiveArray = toPrimitiveArray LineStrip vertexArray
    shader primitiveArray
  swapWindowBuffers win

  closeRequested <- GLFW.windowShouldClose win
  unless (closeRequested == Just True) $
    loop win shader vertexBuffer uniform ((angleRot + 0.005) `mod''` (2*pi)) objectScale

mkScaleTransform :: Float -> M44 Float
mkScaleTransform s =
  (V4 (V4 s 0 0 0)
      (V4 0 s 0 0)
      (V4 0 0 s 0)
      (V4 0 0 0 1)
      )
