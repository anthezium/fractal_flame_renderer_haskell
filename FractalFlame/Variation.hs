module FractalFlame.Variation where

import qualified Data.HashMap.Strict as HMap
import Data.Monoid

import FractalFlame.IFSTypes

applyVariations :: [Variation] -> CartesianPoint -> CartesianPoint
applyVariations []   point = point
applyVariations vars point =
  mconcat $ map (\(Variation coeff transform)-> 
    scalePoint coeff $ transform point) 
                vars

r (Point x y) = sqrt $ x ** 2 + y ** 2

theta (Point x y) = atan (x / y)

rt point = (r point, theta point)

phi (Point x y) = atan (y / x)

linear _ _ _ point = point

sinusoidal _ _ _ point = fmap sin point

spherical _ _ _ point =
  let sc = 1 / (r point) ** 2
  in
    fmap (* sc) point

swirl _ _ _ point@(Point x y) =
  let r2 = (r point) ** 2
      sr2 = sin r2
      cr2 = cos r2
  in
    Point (x * sr2 - y * cr2) (x * cr2 + y * sr2)

horseshoe _ _ _ point@(Point x y) =
  fmap (/ r point) $ Point ((x - y) * (x + y)) (2*x*y)

polar _ _ _ point =
  Point (theta point / pi) (r point - 1)

handkerchief _ _ _ point =
  let theta' = theta point
      r' = r point
  in
    fmap (* r') $ Point (sin $ theta' + r') (cos $ theta' - r')

heart _ _ _ point =
  let theta' = theta point
      r' = r point
      tr = theta' * r'
  in
    fmap (* r') $ Point (sin tr) (- cos tr)

disc _ _ _ point =
  let top = theta point / pi
      pr = pi * r point
  in
    fmap (* top) $ Point (sin pr) (cos pr)

spiral _ _ _ point =
  let ct = cos $ theta point
      st = sin $ theta point
  in
    fmap (/ r point) $ Point (ct + st) (st - ct)

hyperbolic _ _ _ point =
  let (r', theta') = rt point
  in
    Point (sin theta' / r') (r' * cos theta')

diamond _ _ _ point =
  let (rp, tp) = rt point
  in
    Point (sin tp * cos rp) (cos tp * sin rp)

ex _ _ _ point =
  let (rp, tp) = rt point
      p0 = sin $ tp + rp
      p1 = cos $ tp - rp
  in
    fmap (* rp) $ Point (p0 ** 3 + p1 ** 3) (p0 ** 3 - p1 ** 3)

julia _ omega _ point =
  let (rp, tp) = rt point
      a = tp / 2 + omega
  in
    fmap (* sqrt rp) $ Point (cos a) (sin a)

-- I wonder if the compiler will optimize out the unnecessary checks...
bent _ _ _ point@(Point x y)
  | x >= 0 && y >= 0 = point
  | x <  0 && y >= 0 = Point (2 * x)  y
  | x >= 0 && y <  0 = Point      x  (y / 2)
  | x <  0 && y <  0 = Point (2 * x) (y / 2)
  

vars = [
    ("linear", linear)
  , ("sinusoidal", sinusoidal)
  , ("spherical", spherical)
  , ("swirl", swirl)
  , ("horseshoe", horseshoe)
  , ("polar", polar)
  , ("handkerchief", handkerchief)
  , ("heart", heart)
  , ("disc", disc)
  , ("spiral", spiral)
  , ("hyperbolic", hyperbolic)
  , ("diamond", diamond)
  , ("ex", ex)
  , ("julia", julia)
  , ("bent", bent)
  ]
