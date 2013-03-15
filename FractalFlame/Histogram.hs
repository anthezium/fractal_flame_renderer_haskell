module FractalFlame.Histogram where

import Control.Monad
import Control.Monad.ST
import Data.Monoid
import Data.STRef
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Storable.Mutable as MV

import FractalFlame.Camera
import FractalFlame.Flame
import FractalFlame.IFSTypes
import FractalFlame.Palette

plotWithPalette :: Palette -> Coord -> Color -> Color
plotWithPalette palette colorVal color =
  mappend color $ palette colorVal

pointToIndex :: Camera -> CartesianPoint -> Int
pointToIndex camera point =
      --TODO(ted): Fix this kind of ugly naming with per-type modules and -XDisambiguateRecordNames or lenses(?)
  let width = sizeWidth . cameraSize $ camera
      (Point px py) = project camera point
  in
    nFloatChannels * (px + py * width)

-- put this into ColorVec implementation for MVector
--readColor :: ST s (MV.MVector s FloatChannel) -> Int -> ST s Color
readColor vec ix = do
  r <- MV.unsafeRead vec  ix
  g <- MV.unsafeRead vec (ix + 1)
  b <- MV.unsafeRead vec (ix + 2)
  a <- MV.unsafeRead vec (ix + 3)
  {-
  slice <- Unsafe.unsafeFreeze $ MV.unsafeSlice ix nFloatChannels vec 
  [r,g,b,a] <- SV.toList slice
  -}
  return $ Color r g b a

--writeColor :: ST s (MV.MVector s FloatChannel) -> Int -> Color -> ST s ()
writeColor vec ix (Color r g b a) = do
  mapM_ (uncurry $ MV.unsafeWrite vec)
        [(ix    , r)
        ,(ix + 1, g)
        ,(ix + 2, b)
        ,(ix + 3, a)
        ]
  return ()

render :: Camera -> Palette -> FloatChannel -> FloatChannel -> [Plottable] -> PixelFlame
render camera palette vibrancy gamma plottables = 
  let mapping = pointToIndex camera
      width = sizeWidth . cameraSize $ camera
      height = sizeHeight . cameraSize $ camera
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
                           forM_ plottables (\(Plottable point colorVal) -> do
                             let ix = mapping point
                             -- take out this bounds check once it's working
                             when (0 <= ix && ix <= maxIx) $ do 
                               color <- readColor colors ix
                               let ncolor@(Color r g b a) = plot colorVal color
                               acc <- readSTRef amax
                               when (a > acc) $ writeSTRef amax a
                               writeColor colors ix ncolor)
                           amax' <- readSTRef amax
                           colorMap width height (scaleColor amax' . gammaCorrect amax') colors
                           colors' <- SV.unsafeFreeze colors
                           return (amax', colors')
                     
        -- switch to Repa here?, seems like the U (unboxed vectors) representation is appropriate 
        -- scale logarithmically, apply gamma correction, and write out a pixel for each summed color in the histogram
        -- switch to Repa to get automatic parallelization here
        --pixels = fmap (scaleColor amax . gammaCorrect amax) colors 
        pixels = colors
    in PixelFlame width height pixels

colorVecEach width height f vec = do
  let g = f vec
  forM_ [0,nFloatChannels..nFloatChannels*width*height-1] $ flip (>>=) g . return

-- ditch this imperative nonsense after Repa switch
{-
colorMap width height f vec = do
  forM_ [0,nFloatChannels..nFloatChannels*width*height-1] $ (\ix -> do
    color <- readColor vec ix
    writeColor vec ix (f color))
-}

colorMap width height f vec = 
  let f' vec ix = do
                    color <- readColor vec ix
                    writeColor vec ix (f color)
  in
    colorVecEach width height f' vec
