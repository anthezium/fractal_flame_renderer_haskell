module FractalFlame.Histogram 
(render) where

import Control.Monad
import Control.Monad.ST
import Data.Monoid
import Data.STRef
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Storable.Mutable as MV

import FractalFlame.Camera
import FractalFlame.Color
import FractalFlame.Palette.Types.Palette
import FractalFlame.Point.Types.CartesianPoint
import FractalFlame.Point.Types.Point
import FractalFlame.Types.Base
import FractalFlame.Types.PixelFlame
import FractalFlame.Types.Plottable
import FractalFlame.Types.Size

-- | Plot a list of points on a 2D histogram with color indices for each point,
-- | then apply logarithmic scaling and gamma correction.
render :: Camera -- ^ 'camera'.  Specifies the viewport onto which the CartesianPoints are projected.
       -> Palette -- ^ 'palette'. Maps floating-point color indices [0,1] to RGBA Color values.
       -> FloatChannel -- ^ 'vibrancy'. A floating-point value [0,1] that determines how independently the channels in a given pixel are gamma corrected.
       -> FloatChannel -> [Plottable] -> PixelFlame
render camera@(Camera {size = (Size {width, height})}) palette vibrancy gamma plottables = 
  let mapping = pointToIndex camera
      plot = plotWithPalette palette
      gammaCorrect = gammaColor vibrancy gamma 
      maxIx = width * height * nFloatChannels
  in
    let (amax :: FloatChannel, colors :: SV.Vector FloatChannel) = runST $ do
                           -- accumulate color values at points that map to each pixel
                           colors <- MV.new (maxIx - 1) :: ST s (MV.MVector s FloatChannel)
                           --colors <- newArray (0, maxIx) 0 :: ST s (STUArray s Int FloatChannel)
                           -- need to find the maximum alpha to use for scaling all other alpha values
                           amax <- newSTRef floatChannelMin
                           forM_ plottables (\(Plottable point colorIx) -> do
                             let ix = mapping point
                             -- take out this bounds check once it's working
                             when (0 <= ix && ix <= maxIx) $ do 
                               color <- readColor colors ix
                               let ncolor@(Color r g b a) = plot colorIx color
                               acc <- readSTRef amax
                               when (a > acc) $ writeSTRef amax a
                               writeColor colors ix ncolor)
                           amax' <- readSTRef amax
                           -- logarithmic scaling and gamma correction pass on each pixel
                           -- TODO: I think it should be scaleColor, then gammaCorrect.  Refactor those functions to not compute brightness twice (or dereference the same thunk twice or whatever) and verify that result is [0,1]
                           colorMap width height (scaleColor amax' . gammaCorrect amax') colors
                           colors' <- SV.unsafeFreeze colors
                           return (amax', colors')
                     
        -- switch to Repa here?, seems like the U (unboxed vectors) representation is appropriate 
        -- scale logarithmically, apply gamma correction, and write out a pixel for each summed color in the histogram
        -- switch to Repa to get automatic parallelization here
        --pixels = fmap (scaleColor amax . gammaCorrect amax) colors 
        pixels = colors
    in PixelFlame (Size width height) pixels

-- helpers

plotWithPalette :: Palette -> Coord -> Color -> Color
plotWithPalette palette colorIx color =
  mappend color $ palette colorIx

pointToIndex :: Camera -> CartesianPoint -> Int
pointToIndex camera@(Camera {size = (Size {width, height})}) point =
  let (Point px py) = project camera point
  in
    nFloatChannels * (px + py * width)

-- | Read a FractalFlame.Types.Color from the specified vector at index ix
readColor vec ix = do
  r <- MV.unsafeRead vec  ix
  g <- MV.unsafeRead vec (ix + 1)
  b <- MV.unsafeRead vec (ix + 2)
  a <- MV.unsafeRead vec (ix + 3)
  return $ Color r g b a

-- | Write a FractalFlame.Types.Color to the specified vector at index ix
writeColor vec ix (Color r g b a) = do
  mapM_ (uncurry $ MV.unsafeWrite vec)
        [(ix    , r)
        ,(ix + 1, g)
        ,(ix + 2, b)
        ,(ix + 3, a)
        ]
  return ()

-- | Iterate over vec as if it were a vector of FractalFlame.Types.Color
colorVecEach width height f vec = do
  let g = f vec
  forM_ [0,nFloatChannels..nFloatChannels*width*height-1] $ flip (>>=) g . return

-- | Map f over vec as if it were a vector of FractalFlame.Types.Color
colorMap width height f vec = 
  let f' vec ix = do
                    color <- readColor vec ix
                    writeColor vec ix (f color)
  in
    colorVecEach width height f' vec
