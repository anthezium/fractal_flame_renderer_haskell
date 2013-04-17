module FractalFlame.Variation.Types.Variation where

import FractalFlame.Types.Base
import FractalFlame.Variation.Types.VParams
import FractalFlame.Variation.Types.VTransform

data Variation = Variation {
    weight :: Coord
  , vParams :: VParams
  , vTransform :: VTransform
  }
