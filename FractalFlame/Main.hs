module Main where

import Control.Monad
import Data.Array
import qualified Data.HashMap.Strict as HMS
import qualified Data.Vector.Storable as SV
import Test.QuickCheck
import Test.QuickCheck.Gen
import System.Random

import FractalFlame.Camera
import FractalFlame.Color
import FractalFlame.Flam3.Parse
import FractalFlame.Flam3.Types.Flame
import FractalFlame.Flam3.Types.Xform
import FractalFlame.Generator
import FractalFlame.GLDisplay
import FractalFlame.Histogram
import FractalFlame.IFS
import FractalFlame.LinearTransformation
import FractalFlame.Palette
import FractalFlame.Point.Types.Point
import FractalFlame.Types.LinearParams
import FractalFlame.Types.PixelFlame
import FractalFlame.Types.Size
import FractalFlame.Variation

-- Most of the stuff in this module is temporary.  As the renderer moves closer to being able to render flam3s as is,
-- the hardcoded values for xforms, the camera, etc. will be replaced by "parse flam3, render as it specifies, display"

initDemoPalette :: IO Palette
initDemoPalette = do
  flam3 <- parseFlam3 "flam3/electricsheep.244.73060.flam3"
  let fcs = colors flam3
  return $ buildPalette fcs

paramsToXforms =
    map (\(params, colorIx, weight) -> 
      Xform { preParams = (Just params) 
            , postParams = Nothing 
            , colorIx = colorIx
            , weight = weight
            , symmetry = 0
            , variations = []
            , vparams = HMS.empty
            })

blahXforms :: [Xform]
blahXforms = 
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
  in paramsToXforms params

sierpinskiXforms =
  let params = [ ((LinearParams    0.5   0.0   0.0   0.0   0.5   0.0),             0/2, 1)
               , ((LinearParams    0.5   0.0   0.25  0.0   0.5  (0.5*(sqrt 3)/2)), 1/2, 1)
               , ((LinearParams    0.5   0.0   0.5   0.0   0.5   0.0),             2/2, 1)
               ]
  in paramsToXforms params

demoLinear :: Variation
demoLinear = Variation {
    weight = 1
  , vParams = HMS.empty
  , vTransform = vt_linear
  }

demoSpiral :: Variation
demoSpiral = Variation {
    weight = 1
  , vParams = HMS.empty
  , vTransform = vt_spiral
  }

demoSinusoidal :: Variation
demoSinusoidal = Variation {
    weight = 1
  , vParams = HMS.empty
  , vTransform = vt_sinusoidal
  }

demoSwirl :: Variation
demoSwirl = Variation {
    weight = 1
  , vParams = HMS.empty
  , vTransform = vt_swirl
  }

demoDisc :: Variation
demoDisc = Variation {
    weight = 1
  , vParams = HMS.empty
  , vTransform = vt_disc
  }

demoPie :: Variation
demoPie = Variation {
    weight = 0.002
  , vParams = HMS.fromList [("pie_slices", 5), ("pie_rotation", pi / 5), ("pie_thickness", 0.1)]
  , vTransform = vt_pie
  }


swirlPieVariations :: [Variation]
swirlPieVariations = [demoSwirl, demoPie]
--demoVariations = []

-- camera
bigSide = 800
stdSide = 400
sierpinskiSwirlPieDemoBigCamera = Camera { size = (Size bigSide bigSide)
                                         --, center = (Point (-0.2) 0.2)
                                         , center = (Point 0.18 0.38)
                                         , scale = bigSide / 2
                                         , rotate = 0
                                         , zoom = 1.2
                                         }

sierpinskiSwirlStdCamera = Camera { size = (Size stdSide stdSide)
                                  , center = (Point (-0.2) 0.2)
                                  , scale = stdSide / 2
                                  , rotate = 0
                                  , zoom = 1.8
                                  }

sierpinskiSwirlPieBigDemo = (sierpinskiXforms, swirlPieVariations, sierpinskiSwirlPieDemoBigCamera)
sierpinskiSwirlStdDemo = (sierpinskiXforms, [demoSwirl], sierpinskiSwirlStdCamera)

iterationsToDiscard = 20
quality = 20
vibrancy = 0.6
gamma = 3

main :: IO ()
main = do
  s <- newStdGen
  demoPalette <- initDemoPalette
  let (xforms, variations, camera@(Camera {size = (Size width height)})) = sierpinskiSwirlStdDemo
      samples = width * height * quality
      (firstPoint, s') = genFirstPoint s
      (firstColorIx, s'') = genFirstColorIx s'
      firstSeed = s''
      rangeCheck = inCameraCheck camera
      getXform = xformSampler xforms
      final = Nothing
      finalColorIx = Nothing
      -- set up infinite list of plottables
      plottables = take samples $
                    ifs iterationsToDiscard
                        rangeCheck
                        firstPoint
                        firstColorIx
                        firstSeed
                        getXform
                        variations
                        final
                        finalColorIx
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
  forM_ (filter (\(Color r g b a) -> r > 0.2 && g > 0.2 && b > 0.2) . pixelFlame2Colors $ flame) (putStrLn . show)
  -}
  -- display pixels
  displayLoop flame


