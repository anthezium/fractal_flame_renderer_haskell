module FractalFlame.Flame where

import Data.Array
import Data.Int
import Data.Monoid
import qualified Data.Vector.Storable as SV
import Data.Word
import Graphics.UI.GLUT (GLdouble)

type FloatChannel = GLdouble

floatChannelMin = 0 :: FloatChannel
floatChannelMax = 1 :: FloatChannel
nFloatChannels = 4 :: Int

data Color = Color {
    r :: FloatChannel
  , g :: FloatChannel
  , b :: FloatChannel
  , a :: FloatChannel
  } deriving (Show)

instance Monoid Color where
  mempty = (Color 0 0 0 0)
  mappend (Color r1 g1 b1 a1) (Color r2 b2 g2 a2) = 
    Color (r1 + r2) (g1 + g2) (b1 + b2) (a1 + a2)
    
scaleColor :: FloatChannel -> Color -> Color
scaleColor amax (Color r g b a) =
      -- learn more about "functional dispatch" as listed here: http://www.haskell.org/haskellwiki/Currying
      -- are expressions that only depend on supplied parameters evaluated and memoized? (e.g. log amax)
  let brightness = log a / log amax
      aScale = brightness / a
      scale channel = channel * aScale
  in
    Color (scale r) (scale g) (scale b) 1

gammaColor :: FloatChannel -> FloatChannel -> FloatChannel -> Color -> Color
gammaColor vibrancy gamma amax color@(Color r g b a) = 
  let brightness = log a / log amax
      applyGamma component = component ** (1 / gamma)
      alphaGamma = applyGamma brightness
      -- weigh alpha-based and channel-based gamma corrections by vibrancy, more vibrancy == more alpha contribution
      correct channel = channel * (vibrancy * alphaGamma + (1 - vibrancy) * applyGamma channel)
  in
    Color (correct r) (correct g) (correct b) (correct a)

type IntChannel = Word8
intChannelMin = minBound :: IntChannel
intChannelMax = maxBound :: IntChannel

data PixelFlame = PixelFlame {
    flameWidth :: Int
  , flameHeight :: Int
  , pixels :: SV.Vector FloatChannel
  }

flame2Colors :: PixelFlame -> [Color]
flame2Colors (PixelFlame width height pixels) =
  map 
    (\ix -> 
      let [r,g,b,a] = SV.toList $ SV.unsafeSlice ix nFloatChannels pixels 
      in Color r g b a)  
    [0,nFloatChannels..(width*height*nFloatChannels - 1)] 
