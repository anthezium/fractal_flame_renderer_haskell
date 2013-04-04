module Main where

import Control.Monad
import Data.Array
import qualified Data.HashMap.Strict as HMS
import qualified Data.Vector.Storable as SV
import Test.QuickCheck
import Test.QuickCheck.Gen
import System.Random

import FractalFlame.Camera
import FractalFlame.Flame
import FractalFlame.Generator
import FractalFlame.GLDisplay
import FractalFlame.Histogram
import FractalFlame.IFS
import FractalFlame.IFSTypes
import FractalFlame.LinearTransformation
import FractalFlame.Palette
import FractalFlame.ParseFlam3
import FractalFlame.Variation

-- Most of the stuff in this module is temporary.  As the renderer moves closer to being able to render flam3s as is,
-- the hardcoded values for xforms, the camera, etc. will be replaced by "parse flam3, render as it specifies, display"

initDemoPalette :: IO Palette
initDemoPalette = do
  flam3 <- parseFlam3 "flam3/electricsheep.244.73060.flam3"
  let fcs = colors flam3
  return $ buildPalette fcs

paramsToBaseTransforms =
    map (\(params, colorVal, weight) -> 
      (BaseTransform (Just params) Nothing colorVal weight))

blahBaseTransforms :: [BaseTransform]
blahBaseTransforms = 
  let params = [ ((LinearParams   0.5   0.0   0.0   0.0   0.5    0.0 ), 0/5, 1)
               , ((LinearParams   0.5   0.0   0.5   0.0   0.5    0.0 ), 1/5, 1)
               , ((LinearParams   0.5   0.0   0.0   0.0   0.5    0.5 ), 2/5, 1)
               , ((LinearParams (-0.5)  0.0   0.0   0.0   0.5    0.0 ), 3/5, 1)
               , ((LinearParams (-0.5)  0.0 (-0.5)  0.0   0.5    0.0 ), 4/5, 1)
               , ((LinearParams (-0.5)  0.0   0.0   0.0   0.5    0.5 ), 5/5, 1)
               , ((LinearParams (-0.5)  0.0   0.0   0.0 (-0.5)   0.0 ), 5/5, 1)
               , ((LinearParams (-0.5)  0.0 (-0.5)  0.0 (-0.5)   0.0 ), 4/5, 1)
               , ((LinearParams (-0.5)  0.0   0.0   0.0 (-0.5) (-0.5)), 3/5, 1)
               , ((LinearParams   0.5   0.0   0.0   0.0 (-0.5)   0.0 ), 2/5, 1)
               , ((LinearParams   0.5   0.0   0.5   0.0 (-0.5)   0.0 ), 1/5, 1)
               , ((LinearParams   0.5   0.0   0.0   0.0 (-0.5) (-0.5)), 0/5, 1)
               ]
  in paramsToBaseTransforms params

sierpinskiBaseTransforms =
  let params = [ ((LinearParams    0.5   0.0   0.0   0.0   0.5   0.0),             0/2, 1)
               , ((LinearParams    0.5   0.0   0.25  0.0   0.5  (0.5*(sqrt 3)/2)), 1/2, 1)
               , ((LinearParams    0.5   0.0   0.5   0.0   0.5   0.0),             2/2, 1)
               ]
  in paramsToBaseTransforms params

demoLinear :: Variation
demoLinear = Variation {
    variationWeight = 1
  , variationVParams = HMS.empty
  , variationTransform = linear
  }

demoSpiral :: Variation
demoSpiral = Variation {
    variationWeight = 1
  , variationVParams = HMS.empty
  , variationTransform = spiral
  }

demoSinusoidal :: Variation
demoSinusoidal = Variation {
    variationWeight = 1
  , variationVParams = HMS.empty
  , variationTransform = sinusoidal
  }

demoSwirl :: Variation
demoSwirl = Variation {
    variationWeight = 1
  , variationVParams = HMS.empty
  , variationTransform = swirl
  }

demoDisc :: Variation
demoDisc = Variation {
    variationWeight = 1
  , variationVParams = HMS.empty
  , variationTransform = disc
  }


demoVariations :: [Variation]
demoVariations = [demoSwirl]
--demoVariations = []

-- camera
width = 200
height = 200
camera = Camera { cameraSize = (Size width height)
                , cameraCenter = (Point (-0.2) 0.2)
                , cameraScale = 100
                , cameraRotate = 0
                , cameraZoom = 1.9
                }

iterationsToDiscard = 20
quality = 20
samples = width * height * quality
vibrancy = 0.6
gamma = 3

main :: IO ()
main = do
  s <- newStdGen
  demoPalette <- initDemoPalette
  let (s1, s2) = split s
      firstPoint = genFirstPoint s1
      (s3, s4) = split s2
      firstColorVal = genFirstColorVal s3
      rangeCheck = inCameraCheck camera
      baseTransforms = {-# SCC "baseTransforms" #-} sampleBaseTransforms s2 sierpinskiBaseTransforms
      (s5, s6) = split s4
      generators = variationGenerators s5
      variations = demoVariations
      final = Nothing
      finalColorVal = Nothing
      -- set up infinite list of plottables
      plottables = take samples $
                    ifs iterationsToDiscard
                        rangeCheck
                        firstPoint
                        firstColorVal
                        baseTransforms
                        generators
                        variations
                        final
                        finalColorVal
      -- render pixels from plottables
      flame = render camera
                     demoPalette 
                     vibrancy
                     gamma
                     plottables 
{-
  -- print some plottables 
  forM_ (take 100 plottables') (\plottable -> do
    putStrLn $ show plottable)
-}
{-
  -- print bright pixels
  forM_ (filter (\(Color r g b a) -> r > 0.2 && g > 0.2 && b > 0.2) . flame2Colors $ flame) (putStrLn . show)
-}
  -- display pixels
  displayLoop flame


