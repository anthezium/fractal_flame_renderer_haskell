module FractalFlame.Histogram where

import Control.Monad
import Control.Monad.ST
import Data.Array
import Data.Array.ST
import qualified Data.Array.Unsafe as Unsafe
import Data.Monoid
import Data.STRef

import FractalFlame.Camera
import FractalFlame.Flame
import FractalFlame.IFSTypes
import FractalFlame.Palette

plotWithPalette :: Palette -> Coord -> Color -> Color
plotWithPalette palette colorVal color =
  mappend color $ palette colorVal

pointToIndex :: Camera -> Point -> Int
pointToIndex camera point =
      --TODO(ted): Fix this kind of ugly naming with per-type modules and -XDisambiguateRecordNames or lenses(?)
  let width = sizeWidth . cameraSize $ camera
      (GridPoint px py) = project camera point
  in
    px + py * width

render :: Camera -> Palette -> FloatChannel -> FloatChannel -> [Plottable] -> PixelFlame
render camera palette vibrancy gamma plottables = 
  let mapping = pointToIndex camera
      width = sizeWidth . cameraSize $ camera
      height = sizeHeight . cameraSize $ camera
      plot = plotWithPalette palette
      gammaCorrect = gammaColor vibrancy gamma 
      maxIx = width * height
  in
    let (amax :: Double, colors :: Array Int Color) = runST $ do
                           -- accumulate color values at points that map to each pixel
                           -- when I switch to Repa, seems like the V (boxed vectors) representation is appropriate 
                           colors <- newArray (0, maxIx) (Color 0 0 0 0) :: ST s (STArray s Int Color)
                           amax <- newSTRef floatChannelMin
                           forM_ plottables (\(Plottable point colorVal) -> do
                             let ix = mapping point
                             when (0 <= ix && ix <= maxIx) $ do 
                               color <- readArray colors ix
                               let ncolor@(Color r g b a) = plot colorVal color
                               acc <- readSTRef amax
                               when (a > acc) $ writeSTRef amax a
                               writeArray colors ix ncolor)
                           amax' <- readSTRef amax
                           colors' <- Unsafe.unsafeFreeze colors
                           -- does this make a copy on return now that I'm not using runSTArray?
                           return (amax', colors')
                     
        -- scale logarithmically, apply gamma correction, and write out a pixel for each summed color in the histogram
        -- switch to Repa to get automatic parallelization here
        pixels = fmap (scaleColor amax . gammaCorrect amax) colors 
    in PixelFlame width height pixels

