module FractalFlame.LinearTransformation where

import FractalFlame.IFSTypes

linearTransformation :: LinearParams -> Transform
linearTransformation (LinearParams a b c d e f) = 
  (\(Point x y) ->
    Point (a * x + b * y + c)
          (d * x + e * y + f))

