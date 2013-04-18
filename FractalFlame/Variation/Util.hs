module FractalFlame.Variation.Util where

import FractalFlame.Point.Types.CartesianPoint
import FractalFlame.Point.Types.Point
import FractalFlame.Types.Base
import FractalFlame.Variation.Types.VarP as VarP
import FractalFlame.Variation.Types.VParams

-- helpers
-- | Compute a Point's derived data members. 
pointAttrs :: (RealFloat a, Floating a) => Point a -> (a, a, a, a, a)
pointAttrs point@(Point x y) =
  let r = pointRadius point
      theta = pointTheta point
      phi = pointPhi point
  in
    (x, y, r, theta, phi)
  
-- | Update a VarP with a new CartesianPoint and its derived data.
rePointVarP :: VarP -> CartesianPoint -> VarP
rePointVarP varp point =
  let (x, y, r, theta, phi) = pointAttrs point
  in
    -- is there a way to just change point and repoint the dependent thunks in the record?
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

trunc :: Coord -> Coord
trunc = fromIntegral . truncate

flr :: Coord -> Coord
flr = fromIntegral . floor

gaussianRandom :: [Coord] -> Coord
gaussianRandom psis = 
  (sum $ take 4 psis) - 2

-- look at flam3-render to see if this should be truncate
(%) :: Coord -> Coord -> Coord
(%) a b = fromIntegral $ (round a) `mod` (round b)

getVps :: VParams -> String -> [String] -> [Coord]
getVps vparams prefix = map ((vparams !) . ((prefix ++ "_") ++))

