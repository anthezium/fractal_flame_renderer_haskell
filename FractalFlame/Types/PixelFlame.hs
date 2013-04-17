module FractalFlame.Types.PixelFlame where

import qualified Data.Vector.Storable as SV

import FractalFlame.Color.Types.Color
import FractalFlame.Types.Size

data PixelFlame = PixelFlame {
    size :: Size
  , pixels :: SV.Vector FloatChannel
  }

pixelFlame2Colors :: PixelFlame -> [Color]
pixelFlame2Colors (PixelFlame (Size width height) pixels) =
  map 
    (\ix -> 
      let [r,g,b,a] = SV.toList $ SV.unsafeSlice ix nFloatChannels pixels 
      in Color r g b a)  
    [0,nFloatChannels..(width*height*nFloatChannels - 1)] 
