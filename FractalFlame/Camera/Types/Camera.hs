module FractalFlame.Camera.Types.Camera where

import FractalFlame.Point.Types.CartesianPoint
import FractalFlame.Point.Types.GridPoint
import FractalFlame.Types.Base
import FractalFlame.Types.Size

data Camera = Camera {
    size :: Size
  , center :: CartesianPoint
  , scale :: Coord
  , rotate :: Coord
  , zoom :: Coord
  }

