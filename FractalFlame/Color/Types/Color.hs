module FractalFlame.Color.Types.Color where

import Data.Word
import Data.Monoid

type FloatChannel = Double

floatChannelMin = 0 :: FloatChannel
floatChannelMax = 1 :: FloatChannel
nFloatChannels = 4 :: Int


type IntChannel = Word8

intChannelMin = minBound :: Word8
intChannelMax = maxBound :: Word8

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
 
