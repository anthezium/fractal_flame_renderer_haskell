module FractalFlame.Flam3.Types.Color where

import qualified FractalFlame.Color.Types.Color as C

data Color = Color {
    index :: Int
  , rgb :: C.Color
  } deriving (Show)

instance Eq Color where
  f1 == f2 = (index f1) == (index f2)

instance Ord Color where
  f1 `compare` f2 = (index f1) `compare` (index f2)

rgbChannelMin = C.intChannelMin
rgbChannelMax = C.intChannelMax
