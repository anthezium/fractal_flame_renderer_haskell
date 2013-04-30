module Main where

import Control.Monad
import System.Random

import FractalFlame.Camera
import FractalFlame.Color
import FractalFlame.Flam3.Flame
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

initFlame :: String -> StdGen -> IO (Flame, StdGen)
initFlame path s = do
  flame <- parseFlam3 path
  return $ postProcessFlame s flame

iterationsToDiscard = 20

main :: IO ()
main = do
  s <- newStdGen
  (flame@(Flame { xforms, finalXform
               , camera = camera@(Camera {size = (Size width height)})
               , quality, vibrancy, gamma
               , colors = (ColorPalette palette)
               }), s') <- initFlame "flam3/sierpinski_demo_medium.flam3" s
  let samples = width * height * quality
      (firstPoint, s'') = genFirstPoint s'
      (firstColorIx, s''') = genFirstColorIx s''
      firstSeed = s'''
      rangeCheck = inCameraCheck camera
      getXform = xformSampler xforms
      -- set up infinite list of plottables
      plottables = take samples $
                    ifs iterationsToDiscard
                        rangeCheck
                        firstPoint
                        firstColorIx
                        firstSeed
                        getXform
                        finalXform
      -- render pixels from plottables
      flame = render camera
                     palette 
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


