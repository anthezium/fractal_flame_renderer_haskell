module LinearTransformation where

import IFSTypes

linearTransformation :: LinearParams -> Transform
linearTransformation (LinearParams xx xy xc yx yy yc) = 
  (\(Point x y) ->
    Point (xx * x + xy * y + xc)
          (yx * x + yy * y + yc))

