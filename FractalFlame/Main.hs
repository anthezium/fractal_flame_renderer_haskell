module Main where

import Test.QuickCheck
import Test.QuickCheck.Gen
import System.Random

import FractalFlame.Camera
import FractalFlame.Generator
import FractalFlame.GLDisplay
import FractalFlame.Histogram
import FractalFlame.IFS
import FractalFlame.IFSTypes
import FractalFlame.LinearTransformation

demoPalette colorVal = (Color (sin $ 2*pi*colorVal) (cos $ 2*pi*colorVal) (tan $ 2*pi*colorVal) 1)

demoBaseTransforms :: [BaseTransform]
demoBaseTransforms = 
{-
  let params = [ ((LinearParams   0.5   0.0   0.0   0.0   0.5    0.0 ), 0/5)
               , ((LinearParams   0.5   0.0   0.5   0.0   0.5    0.0 ), 1/5)
               , ((LinearParams   0.5   0.0   0.0   0.0   0.5    0.5 ), 2/5)
               , ((LinearParams (-0.5)  0.0   0.0   0.0   0.5    0.0 ), 3/5)
               , ((LinearParams (-0.5)  0.0 (-0.5)  0.0   0.5    0.0 ), 4/5)
               , ((LinearParams (-0.5)  0.0   0.0   0.0   0.5    0.5 ), 5/5)
               , ((LinearParams (-0.5)  0.0   0.0   0.0 (-0.5)   0.0 ), 5/5)
               , ((LinearParams (-0.5)  0.0 (-0.5)  0.0 (-0.5)   0.0 ), 4/5)
               , ((LinearParams (-0.5)  0.0   0.0   0.0 (-0.5) (-0.5)), 3/5)
               , ((LinearParams   0.5   0.0   0.0   0.0 (-0.5)   0.0 ), 2/5)
               , ((LinearParams   0.5   0.0   0.5   0.0 (-0.5)   0.0 ), 1/5)
               , ((LinearParams   0.5   0.0   0.0   0.0 (-0.5) (-0.5)), 0/5)
               ]
-}
  let params = [ ((LinearParams    0.5   0.0   0.0   0.0   0.5   0.0 ), 0/2)
               , ((LinearParams    0.5   0.0   0.25  0.0   0.5  (0.5*(sqrt 3)/2)), 1/2)
               , ((LinearParams    0.5   0.0   0.5   0.0   0.5   0.0), 2/2)
               ]
  in
    map (\(params, colorVal) -> (BaseTransform (Just $ linearTransformation params) Nothing colorVal 1)) params

demoVariations :: [Variation]
demoVariations = []

-- camera
width = 320
height = 320
camera = Camera { cameraSize = (Size width height)
                , cameraCenter = (Point 0.5 0.5)
                , cameraScale = 160
                , cameraRotate = 0
                , cameraZoom = 1
                }

iterationsToDiscard = 20
quality = 12
samples = width * height * quality
vibrancy = 0.5
gamma = 2.2

-- rangeCheck :: Point -> Bool
-- rangeCheck (Point x y) = -1 <= x && x <= 1 && -1 <= y && y <= 1

main :: IO ()
main = do
  s <- newStdGen
  let (s1, s2) = split s
      firstPoint = genFirstPoint s1
      (s3, s4) = split s2
      firstColorVal = genFirstColorVal s3
      rangeCheck = inCameraCheck camera
      baseTransforms = sampleBaseTransforms s2 demoBaseTransforms
      variations = demoVariations
      final = Nothing
      finalColorVal = Nothing
      -- create infinite list of plottables
      plottables = ifs iterationsToDiscard
                       rangeCheck
                       firstPoint
                       firstColorVal
                       baseTransforms
                       variations
                       final
                       finalColorVal
      -- render pixels from plottables
      flame = render camera
                     demoPalette 
                     vibrancy
                     gamma
                     (take samples plottables) 
  -- display pixels
  displayLoop flame


