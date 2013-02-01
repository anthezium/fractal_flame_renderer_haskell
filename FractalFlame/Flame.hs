module Flame (
    Pixel
  , PixelFlame
  ) where

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
    width :: Int
  , height :: Int
  , pixels :: Array Int Pixel --should switch this to unboxed array of bytes at some point for performance
  }


