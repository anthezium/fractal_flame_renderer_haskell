module FractalFlame.Flame where

import Data.Array
--import Data.Array.IArray
import Data.Int
import Data.Word

type IntChannel = Word8
intChannelMax = maxBound :: IntChannel

type Count = Int64

data Pixel = Pixel {
    r :: IntChannel
  , g :: IntChannel
  , b :: IntChannel
  }

data PixelFlame = PixelFlame {
    flameWidth :: Int
  , flameHeight :: Int
  , pixels :: Array Int Pixel --should switch this to unboxed array of bytes at some point for performance
  }
