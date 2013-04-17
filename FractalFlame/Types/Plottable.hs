module FractalFlame.Types.Plottable where

import FractalFlame.Point.Types.CartesianPoint
import FractalFlame.Types.Base

data Plottable = Plottable {
    point :: CartesianPoint
  , colorIx :: Coord
  } deriving (Show)

