module FractalFlame.Flam3.Types.Flame where

import FractalFlame.Flam3.Types.Xform
import FractalFlame.Flam3.Types.Color
import FractalFlame.Types.Size

data Flame = Flame {
    colors     :: [Color]
  , xforms     :: [Xform]
  , finalxform :: Maybe Xform
  , size       :: Size
  }

