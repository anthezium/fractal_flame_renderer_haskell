module FractalFlame.Histogram where

import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Monoid

import FractalFlame.Camera
import FractalFlame.Flame
import FractalFlame.IFSTypes


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

colorToPixel :: Color -> Pixel
colorToPixel (Color r g b a) =
  (Pixel
    (convert r)
    (convert g)
    (convert b))
  where convert channel = truncate $ channel * fromIntegral intChannelMax

render :: Camera -> Palette -> FloatChannel -> FloatChannel -> [Plottable] -> PixelFlame
render camera palette vibrancy gamma plottables = 
  let mapping = pointToIndex camera
      width = sizeWidth . cameraSize $ camera
      height = sizeHeight . cameraSize $ camera
      plot = plotWithPalette palette
      gammaCorrect = gammaColor vibrancy gamma 
      maxIx = width * height
  in
    let colors = runSTArray $ do
                   -- accumulate color values at points that map to each pixel
                   -- when I switch to Repa, seems like the V (boxed vectors) representation is appropriate 
                   colors <- newArray (0, maxIx) (Color 0 0 0 0) :: ST s (STArray s Int Color)
                   forM_ plottables (\(Plottable point colorVal) -> do
                     let ix = mapping point
                     when (0 <= ix && ix <= maxIx) $ do 
                       color <- readArray colors ix
                       writeArray colors ix $ plot colorVal color)
                   return colors
                     
        -- scale logarithmically, apply gamma correction, and write out a pixel for each summed color in the histogram
        -- switch to Repa to get automatic parallelization here
        pixels = fmap (colorToPixel . gammaCorrect . scaleColor) colors 
    in PixelFlame width height pixels

