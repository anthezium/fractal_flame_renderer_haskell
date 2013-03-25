module FractalFlame.Variation where

import Data.HashMap.Strict (HashMap, (!))
import Data.Monoid

import FractalFlame.IFSTypes

-- exported
data VarP = VarP {
    psis :: [Coord]
  , omegas :: [Coord]
  , lambdas :: [Coord]
  , linearParams :: LinearParams
  , vparams :: HashMap String Coord
  , weight :: Coord
  }

applyVariations :: [Variation] -> CartesianPoint -> CartesianPoint
applyVariations []   point = point
applyVariations vars point =
  mconcat $ map (\(Variation weight transform)-> 
    scalePoint weight $ transform point) 
                vars

-- helpers
radius (Point x y) = sqrt $ x^2 + y^2

-- did I get this right?  Should be the angle from the x axis.  opposite over adjacent...
theta (Point x y) = atan2 y x

rt point = (radius point, theta point)

phi (Point x y) = atan2 x y

trunc = fromIntegral . truncate

flr = fromIntegral . floor

gaussianR psis = 
  let gr = (sum $ take 4 psis) - 2
      psis' = drop 4 psis
  in
    (gr, psis')

-- look at flam3-render to see if this should be truncate
(%) a b = fromIntegral $ (round a) `mod` (round b)

getVps vparams names = map (vparams !) names

-- Variations
linear _ point = point

sinusoidal _ point = fmap sin point

spherical _ point =
  let sc = 1 / (radius point)^2
  in
    fmap (* sc) point

swirl _ point@(Point x y) =
  let r2 = (radius point)^2
      sr2 = sin r2
      cr2 = cos r2
  in
    Point (x * sr2 - y * cr2) (x * cr2 + y * sr2)

horseshoe _ point@(Point x y) =
  fmap (/ radius point) $ Point ((x - y) * (x + y)) (2*x*y)

polar _ point =
  Point (theta point / pi) (radius point - 1)

handkerchief _ point =
  let theta' = theta point
      r' = radius point
  in
    fmap (* r') $ Point (sin $ theta' + r') (cos $ theta' - r')

heart _ point =
  let theta' = theta point
      r' = radius point
      tr = theta' * r'
  in
    fmap (* r') $ Point (sin tr) (- cos tr)

disc _ point =
  let top = theta point / pi
      pr = pi * radius point
  in
    fmap (* top) $ Point (sin pr) (cos pr)

spiral _ point =
  let ct = cos $ theta point
      st = sin $ theta point
  in
    fmap (/ radius point) $ Point (ct + st) (st - ct)

hyperbolic _ point =
  let (r', theta') = rt point
  in
    Point (sin theta' / r') (r' * cos theta')

diamond _ point =
  let (r, th) = rt point
  in
    Point (sin th * cos r) (cos th * sin r)

ex _ point =
  let (r, th) = rt point
      p0 = sin $ th + r
      p1 = cos $ th - r
  in
    fmap (* r) $ Point (p0^3 + p1^3) (p0^3 - p1^3)

julia (VarP {omegas}) point =
  let (omega:_) = omegas
      (r, th) = rt point
      a = th / 2 + omega
  in
    fmap (* sqrt r) $ Point (cos a) (sin a)


-- I wonder if the compiler will optimize out the unnecessary checks...
bent _ point@(Point x y)
  | x >= 0 && y >= 0 = point
  | x <  0 && y >= 0 = Point (2 * x)  y
  | x >= 0 && y <  0 = Point      x  (y / 2)
  | x <  0 && y <  0 = Point (2 * x) (y / 2)
  
waves (VarP {linearParams}) point@(Point x y) =
  let (LinearParams _ b c _ e f) = linearParams
  in
    Point (x + b * sin (y / c^2)) (y + e * sin (x / f^2))

fisheye _ point@(Point x y) = eyefish undefined $ Point y x

popcorn (VarP {linearParams}) point@(Point x y) =
  let (LinearParams _ _ c _ _ f) = linearParams
  in
    Point (x + c * (sin . tan $ 3 * y)) (y + f * (sin . tan $ 3 * x))

exponential _ point@(Point x y) =
  fmap (* (exp $ x - 1)) $ Point (cos $ pi * y) (sin $ pi * y)

power _ point@(Point x y) =
  let (r, th) = rt point
  in
    fmap (* r**(sin th)) $ Point (cos th) (sin th)

cosine _ point@(Point x y) =
  Point (cos (pi * x) * cosh y) (- 1 * sin (pi * x) * sinh y)

rings (VarP {linearParams}) point@(Point x y) =
  let (LinearParams _ _ c _ _ _) = linearParams
      (r, th) = rt point
      m = (r + c^2) % (2 * c^2) - r * (1 - c^2)
  in
    fmap (* m) $ Point (cos th) (sin th)

fan (VarP {linearParams}) point@(Point x y) =
  let (LinearParams _ _ c _ _ f) = linearParams
      t = pi * c^2
      (r, th) = rt point
      g z = fmap (* r) $ Point (cos z) (sin z)
  in
    if (th + f) % t > t / 2 then
      g $ th - t / 2
    else
      g $ th + t / 2

eyefish _ point =
  let r = radius point
  in
    fmap (* (2 / (r + 1))) point

bubble _ point@(Point x y) =
  let r = radius point
  in
    fmap (* (4 / (r^2 + 4))) point

cylinder _ (Point x y) = Point (sin x) y

noise (VarP {psis}) (Point x y) =
  let (psi1:psi2:_) = psis
      z = 2 * pi * psi2
  in
    fmap (* psi1) $ Point (x * cos z) (y * sin z)

blur (VarP {psis}) _ =
  let (psi1:psi2:_) = psis
      z = 2 * pi * psi2
  in
    fmap (* psi1) $ Point (cos z) (sin z)

rings2 (VarP {vparams}) point =
  let [v] = getVps vparams ["rings2_val"]
      p = v^2
      (r, th) = rt point
      t = r - 2 * p * trunc ((r + p) / (2 * p)) + r * (1 - p)
  in
    fmap (* t) $ Point (sin th) (cos th)

fan2 (VarP {vparams}) point =
  let [vx, vy] = getVps vparams ["fan2_x", "fan2_y"]
      p1 = pi * vx^2
      p2 = vy
      (r, th) = rt point
      t = th + p2 - p1 * trunc (2 * th * p2 / p1)
      g z = fmap (* r) $ Point (sin z) (cos z)
  in
    if t > p1 / 2 then
      g $ th - p1 / 2
    else
      g $ th + p1 / 2

blob (VarP {vparams}) point =
  let [high, low, waves] = getVps vparams ["blob_high", "blob_low", "blob_waves"]
      (r, th) = rt point
      m = r * (low + ((high - low) / 2) * (sin (waves * th) + 1))
  in
    fmap (* m) $ Point (cos th) (sin th)

pdj (VarP {vparams}) (Point x y) =
  let [a,b,c,d] = getVps vparams ["pdj_a", "pdj_b", "pdj_c", "pdj_d"]
  in
    Point ((sin $ a * y) - (cos $ b * x)) ((sin $ c * x) - (cos $ d * y)) 

perspective (VarP {vparams}) (Point x y) =
  let [angle, dist] = getVps vparams ["perspective_angle", "perspective_dist"]
      m = dist / (dist - y * sin angle)
  in
    fmap (* m) $ Point x (y * cos angle)

julian (VarP {psis, vparams}) point =
  let (psi:_) = psis
      [power, dist] = getVps vparams ["julian_power", "julian_dist"]
      phip = phi point
      r = radius point
      p = trunc $ (abs power) * psi
      t = (phip + 2 * pi * p) / power
      m = r**(dist / power)
  in
    fmap (* m) $ Point (cos t) (sin t)

juliascope (VarP {psis, lambdas, vparams}) point =
  let (psi:_) = psis
      (lambda:_) = lambdas
      [power, dist] = getVps vparams ["juliascope_power", "juliascope_dist"]
      phip = phi point
      r = radius point
      p = trunc $ (abs power) * psi
      t = (lambda * phip + 2 * pi * p) / power
      m = r**(dist / power)
  in
    fmap (* m) $ Point (cos t) (sin t)

gaussian (VarP {psis}) point =
  let (dist, (psi5:_)) = gaussianR psis 
      z = 2 * pi * psi5
  in
    fmap (* dist) $ Point (cos z) (sin z)

-- ask spot what the (1/v36) in the paper means
-- following spec on wiki for now
radialblur (VarP {psis, vparams, weight}) point@(Point x y) =
  let [angle] = getVps vparams ["radialblur_angle"]
      p = angle * pi / 2
      (dist, _) = gaussianR psis
      phip = phi point
      r = radius point
      t1 = dist
      t2 = phip + t1 * sin p
      t3 = t1 * cos p - 1
  in
    Point (r * cos t2 + t3 * x) (r * sin t2 + t3 * y)

pie (VarP {psis, vparams}) _ =
  let (psi1:psi2:psi3:_) = psis
      [slices, rotation, thickness] = getVps vparams ["pie_slices", "pie_rotation", "pie_thickness"]
      t1 = trunc $ psi1 * slices + 0.5
      t2 = rotation + 2 * pi / slices * (t1 + psi2 * thickness)
  in
    fmap (* psi3) $ Point (cos t2) (sin t2)

ngon (VarP {vparams}) point =
  let [power, sides, corners, circle] = getVps vparams ["ngon_power", "ngon_sides", "ngon_corners", "ngon_circle"]
      phip = phi point
      r = radius point
      p = 2 * pi / sides
      t3 = phip - p * flr (phip / p)
      t4 = if t3 > p / 2 then t3 else t3 - p
      k = (corners * ((1 / cos t4) - 1) + circle) / r**power
  in
    fmap (* k) $ point  

curl (VarP {vparams}) (Point x y) =
  let [c1, c2] = getVps vparams ["curl_c1", "curl_c2"]
      t1 = 1 + c1 * x + c2 * (x^2 - y^2)
      t2 = c1 * y + 2 * c2 * x * y
      m = 1 / (t1^2 + t2^2)
  in
    fmap (* m) $ Point (x * t1 + y * t2) (y * t1 - x * t2)

rectangles (VarP {vparams}) (Point x y) =
  let [rx, ry] = getVps vparams ["rectangles_x", "rectangles_y"]
  in
    Point ((2 * flr (x / rx) + 1) * rx - x) ((2 * flr (y / ry) + 1) * ry - y)

-- what does "v41" mean in the paper?  OH, I guess it's the weight for this variation.  shoot, another parameter.

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
  , ("pdj", pdj)
  , ("perspective", perspective)
  , ("julian", julian)
  , ("juliascope", juliascope)
  , ("blur", blur)
  , ("gaussian", gaussian)
  , ("radialblur", radialblur)
  , ("pie", pie)
  , ("ngon", ngon)
  , ("curl", curl)
  , ("rectangles", rectangles)
  ]
  
