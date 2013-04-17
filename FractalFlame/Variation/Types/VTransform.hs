module FractalFlame.Variation.Types.VTransform where

import FractalFlame.Point.Types.CartesianPoint
import FractalFlame.Variation.Types.VarP

-- | Variation function (use FractalFlame.Variation.runVariation to build a VarP parameter)
type VTransform = VarP -> CartesianPoint

