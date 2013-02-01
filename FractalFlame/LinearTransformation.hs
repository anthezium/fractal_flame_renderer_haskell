module LinearTransformation where

import IFSTypes

linearTransformation :: LinearParams -> Transform
linearTransformation (LinearParams xx xy xc yx yy yc) = 
  (\(Point x y) ->
    Point (xx * x + xy * y + xc)
          (yx * x + yy * y + yc))

transformList :: StdGen -> [LinearPair] -> [LinearPair]
transformList s pairs =
  unGen (oneof pairs) s 1

demoTransforms :: [Transform]
demoTransforms = 
  let params = [
      (LinearParams  0.5  0.0  0.0  0.0  0.5  0.0)
    , (LinearParams  0.5  0.0  0.5  0.0  0.5  0.0)
    , (LinearParams  0.5  0.0  0.0  0.0  0.5  0.5)
    , (LinearParams -0.5  0.0  0.0  0.0  0.5  0.0)
    , (LinearParams -0.5  0.0 -0.5  0.0  0.5  0.0)
    , (LinearParams -0.5  0.0  0.0  0.0  0.5  0.5)
    , (LinearParams -0.5  0.0  0.0  0.0 -0.5  0.0)
    , (LinearParams -0.5  0.0 -0.5  0.0 -0.5  0.0)
    , (LinearParams -0.5  0.0  0.0  0.0 -0.5 -0.5)
    , (LinearParams  0.5  0.0  0.0  0.0 -0.5  0.0)
    , (LinearParams  0.5  0.0  0.5  0.0 -0.5  0.0)
    , (LinearParams  0.5  0.0  0.0  0.0 -0.5 -0.5)
    ]
  in
    map linearTransformation params

demoPairs :: [LinearPair]
demoPairs =
  map (\transform -> LinearPair (Just transform) Nothing) demoTransforms
