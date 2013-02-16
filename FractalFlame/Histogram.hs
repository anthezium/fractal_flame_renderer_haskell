module Histogram where

import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Monoid

import Flame
import IFSTypes

{- get some performance out of assuming the action is in ([-1,1],[-1,1]) -}
pointToIndex width height (Point x y) =
  let wshift = fromIntegral (div width 2) :: Coord
      hshift = fromIntegral (div height 2) :: Coord
      x' = x
      y' = (-1) * y -- flip it upside-down since increasing height is down
      px = truncate ((x' * wshift) + wshift) :: Int 
      py = truncate ((y' * hshift) + hshift) :: Int
  in 
    px + py * width

plotWithPalette :: Palette -> Coord -> Color -> Color
plotWithPalette palette colorVal color =
  mappend color $ palette colorVal

colorToPixel :: Color -> Pixel
colorToPixel (Color r g b a) =
  (Pixel
    (convert r)
    (convert g)
    (convert b))
  where convert channel = truncate $ channel * fromIntegral intChannelMax

render :: Int -> Int -> Palette -> FloatChannel -> FloatChannel -> [Plottable] -> PixelFlame
render width height palette vibrancy gamma plottables = 
  let mapping = pointToIndex width height
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

