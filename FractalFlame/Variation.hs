module FractalFlame.Variation where

import Data.HashMap.Strict ((!))
import Data.Monoid

import FractalFlame.IFSTypes

applyVariations :: [Variation] -> CartesianPoint -> CartesianPoint
applyVariations []   point = point
applyVariations vars point =
  mconcat $ map (\(Variation coeff transform)-> 
    scalePoint coeff $ transform point) 
                vars

radius (Point x y) = sqrt $ x ** 2 + y ** 2

theta (Point x y) = atan (x / y)

rt point = (radius point, theta point)

phi (Point x y) = atan (y / x)

trunc = fromIntegral . truncate

-- look at flam3-render to see if this should be truncate
(%) a b = fromIntegral $ (round a) `mod` (round b)

linear _ _ _ _ _ point = point

sinusoidal _ _ _ _ _ point = fmap sin point

spherical _ _ _ _ _ point =
  let sc = 1 / (radius point) ** 2
  in
    fmap (* sc) point

swirl _ _ _ _ _ point@(Point x y) =
  let r2 = (radius point) ** 2
      sr2 = sin r2
      cr2 = cos r2
  in
    Point (x * sr2 - y * cr2) (x * cr2 + y * sr2)

horseshoe _ _ _ _ _ point@(Point x y) =
  fmap (/ radius point) $ Point ((x - y) * (x + y)) (2*x*y)

polar _ _ _ _ _ point =
  Point (theta point / pi) (radius point - 1)

handkerchief _ _ _ _ _ point =
  let theta' = theta point
      r' = radius point
  in
    fmap (* r') $ Point (sin $ theta' + r') (cos $ theta' - r')

heart _ _ _ _ _ point =
  let theta' = theta point
      r' = radius point
      tr = theta' * r'
  in
    fmap (* r') $ Point (sin tr) (- cos tr)

disc _ _ _ _ _ point =
  let top = theta point / pi
      pr = pi * radius point
  in
    fmap (* top) $ Point (sin pr) (cos pr)

spiral _ _ _ _ _ point =
  let ct = cos $ theta point
      st = sin $ theta point
  in
    fmap (/ radius point) $ Point (ct + st) (st - ct)

hyperbolic _ _ _ _ _ point =
  let (r', theta') = rt point
  in
    Point (sin theta' / r') (r' * cos theta')

diamond _ _ _ _ _ point =
  let (r, th) = rt point
  in
    Point (sin th * cos r) (cos th * sin r)

ex _ _ _ _ _ point =
  let (r, th) = rt point
      p0 = sin $ th + r
      p1 = cos $ th - r
  in
    fmap (* r) $ Point (p0 ** 3 + p1 ** 3) (p0 ** 3 - p1 ** 3)

julia _ (omega:_) _ _ _ point =
  let (r, th) = rt point
      a = th / 2 + omega
  in
    fmap (* sqrt r) $ Point (cos a) (sin a)

-- I wonder if the compiler will optimize out the unnecessary checks...
bent _ _ _ _ _ point@(Point x y)
  | x >= 0 && y >= 0 = point
  | x <  0 && y >= 0 = Point (2 * x)  y
  | x >= 0 && y <  0 = Point      x  (y / 2)
  | x <  0 && y <  0 = Point (2 * x) (y / 2)
  
waves _ _ _ (LinearParams _ b c _ e f) _ point@(Point x y) =
  Point (x + b * sin (y / c**2)) (y + e * sin (x / f**2))

fisheye _ _ _ _ _ point@(Point x y) = eyefish undefined undefined undefined undefined undefined $ Point y x

popcorn _ _ _ (LinearParams _ _ c _ _ f) _ point@(Point x y) =
  Point (x + c * (sin . tan $ 3 * y)) (y + f * (sin . tan $ 3 * x))

exponential _ _ _ _ _ point@(Point x y) =
  fmap (* (exp $ x - 1)) $ Point (cos $ pi * y) (sin $ pi * y)

power _ _ _ _ _ point@(Point x y) =
  let (r, th) = rt point
  in
    fmap (* r**(sin th)) $ Point (cos th) (sin th)

cosine _ _ _ _ _ point@(Point x y) =
  Point (cos (pi * x) * cosh y) (- 1 * sin (pi * x) * sinh y)

rings _ _ _ (LinearParams _ _ c _ _ _) _ point@(Point x y) =
  let (r, th) = rt point
      m = (r + c**2) % (2 * c**2) - r * (1 - c**2)
  in
    fmap (* m) $ Point (cos th) (sin th)

fan _ _ _ (LinearParams _ _ c _ _ f) _ point@(Point x y) =
  let t = pi * c**2
      (r, th) = rt point
      g z = fmap (* r) $ Point (cos z) (sin z)
  in
    if (th + f) % t > t / 2 then
      g $ th - t / 2
    else
      g $ th + t / 2

eyefish _ _ _ _ _ point =
  let r = radius point
  in
    fmap (* (2 / (r + 1))) point

bubble _ _ _ _ _ point@(Point x y) =
  let r = radius point
  in
    fmap (* (4 / (r**2 + 4))) point

cylinder _ _ _ _ _ (Point x y) = Point (sin x) y

noise (psi1:psi2:_) _ _ _ _ (Point x y) =
  let z = 2 * pi * psi2
  in
    fmap (* psi1) $ Point (x * cos z) (y * sin z)

blur (psi1:psi2:_) _ _ _ _ _ =
  let z = 2 * pi * psi2
  in
    fmap (* psi1) $ Point (cos z) (sin z)

rings2 _ _ _ _ vparams point =
  let v = vparams ! "rings2_val"
      p = v**2
      (r, th) = rt point
      t = r - 2 * p * trunc ((r + p) / (2 * p)) + r * (1 - p)
  in
    fmap (* t) $ Point (sin th) (cos th)

fan2 _ _ _ _ vparams point =
  let vx = vparams ! "fan2_x"
      vy = vparams ! "fan2_y"
      p1 = pi * vx**2
      p2 = vy
      (r, th) = rt point
      t = th + p2 - p1 * trunc (2 * th * p2 / p1)
      g z = fmap (* r) $ Point (sin z) (cos z)
  in
    if t > p1 / 2 then
      g $ th - p1 / 2
    else
      g $ th + p1 / 2

blob _ _ _ _ vparams point =
  let high = vparams ! "blob_high"
      low = vparams ! "blob_low"
      waves = vparams ! "blob_waves"
      (r, th) = rt point
      m = r * (low + ((high - low) / 2) * (sin (waves * th) + 1))
  in
    fmap (* m) $ Point (cos th) (sin th)
    

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
  , ("waves", waves)
  , ("fisheye", fisheye)
  , ("popcorn", popcorn)
  , ("exponential", exponential)
  , ("power", power)
  , ("cosine", cosine)
  , ("rings", rings)
  , ("fan", fan)
  , ("eyefish", eyefish)
  , ("bubble", bubble)
  , ("cylinder", cylinder)
  , ("noise", noise)
  , ("blur", blur)
  , ("rings2", rings2)
  , ("fan2", fan2)
  ]
