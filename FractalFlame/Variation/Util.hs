module FractalFlame.Variation.Util where

import FractalFlame.Point.Types.Point
import FractalFlame.Types.Base
import FractalFlame.Variation.Types.VarP as VarP
import FractalFlame.Variation.Types.VParams

-- helpers
pointAttrs :: (RealFloat a, Floating a) => Point a -> (a, a, a, a, a, Point a)
pointAttrs point@(Point x y) =
  let r = pointRadius point
      theta = pointTheta point
      phi = pointPhi point
  in
    (x, y, r, theta, phi, point)
  
-- is there a way to just change point and repoint the dependent thunks in the record?
rePointVarP :: VarP -> Point a -> VarP
rePointVarP varp point =
  let (x, y, r, theta, phi, point) = pointAttrs point
  in
    varp {
        VarP.point = point
      , VarP.x = x
      , VarP.y = y
      , VarP.r = r
      , VarP.theta = theta
      , VarP.phi = phi
      }

pointRadius (Point x y) = sqrt $ x^2 + y^2

-- did I get this right?  Should be the angle from the x axis.  opposite over adjacent...
pointTheta (Point x y) = atan2 y x

pointPhi (Point x y) = atan2 x y

trunc = fromIntegral . truncate

flr = fromIntegral . floor

gaussianRandom psis = 
  (sum $ take 4 psis) - 2

-- look at flam3-render to see if this should be truncate
(%) a b = fromIntegral $ (round a) `mod` (round b)

getVps :: VParams -> String -> [String] -> [Coord]
getVps vparams prefix = map ((vparams !) . ((prefix ++ "_") ++))

