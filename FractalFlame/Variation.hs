module FractalFlame.Variation where

import Data.HashMap.Strict (HashMap, (!))
import Data.Monoid

import FractalFlame.IFSTypes hiding (x, y)

-- exported
-- should I worry about the number of fields w.r.t. efficiency?  will building all those thunks take extra time or use a significant amount of extra memory?  VarPs are only bound in the scope of runVariation, so they should be garbage collected when it returns.  This should happen automatically since each new point is eagerly evaluated and that should evaluate all the thunks that are needed and let the current VarP go out of scope.
data VarP = VarP {
    psis         :: [Coord]
  , psi1         :: Coord
  , psi2         :: Coord
  , psi3         :: Coord
  , psi4         :: Coord
  , psi5         :: Coord
  , omegas       :: [Coord]
  , omega1       :: Coord
  , omega2       :: Coord
  , omega3       :: Coord
  , omega4       :: Coord
  , omega5       :: Coord
  , lambdas      :: [Coord]
  , lambda1      :: Coord
  , lambda2      :: Coord
  , lambda3      :: Coord
  , lambda4      :: Coord
  , lambda5      :: Coord
  , linearParams :: LinearParams
  , a            :: Coord
  , b            :: Coord
  , c            :: Coord
  , d            :: Coord
  , e            :: Coord
  , f            :: Coord
  , vparams      :: HashMap String Coord
  , weight       :: Coord
  , x            :: Coord
  , y            :: Coord
  , r            :: Coord
  , theta        :: Coord
  , phi          :: Coord
  , point        :: CartesianPoint
  }

type VParams = HashMap String Coord

-- better way than typing all of this stuff?  template haskell?
runVariation :: [Coord] -> [Coord] -> [Coord] -> LinearParams -> VParams -> Coord -> (VarP -> CartesianPoint) -> CartesianPoint -> CartesianPoint
runVariation psis omegas lambdas linearParams@(LinearParams a b c d e f) vparams weight var point@(Point x y) =
  let (psi1:psi2:psi3:psi4:psi5:psis') = psis
      (omega1:omega2:omega3:omega4:omega5:omegas') = omegas 
      (lambda1:lambda2:lambda3:lambda4:lambda5:lambdas') = lambdas 
      (x, y, r, theta, phi, point) = pointAttrs point
      -- how can I include gaussianR in this without taking the preceding psis when I don't need to?
  in
    -- is there a way to avoid typing each name twice?
    var $ VarP {
         psis = psis
      ,  psi1 = psi1
      ,  psi2 = psi2
      ,  psi3 = psi3
      ,  psi4 = psi4
      ,  psi5 = psi5
      ,  omegas = omegas
      ,  omega1 = omega1
      ,  omega2 = omega2
      ,  omega3 = omega3
      ,  omega4 = omega4
      ,  omega5 = omega5
      ,  lambdas = lambdas
      ,  lambda1 = lambda1
      ,  lambda2 = lambda2
      ,  lambda3 = lambda3
      ,  lambda4 = lambda4
      ,  lambda5 = lambda5
      ,  linearParams = linearParams
      ,  a = a
      ,  b = b
      ,  c = c
      ,  d = d
      ,  e = e
      ,  f = f
      ,  vparams = vparams
      ,  weight = weight
      ,  x = x
      ,  y = y
      ,  r = r
      ,  theta = theta
      ,  phi = phi
      ,  point = point
      }


applyVariations :: [Variation] -> CartesianPoint -> CartesianPoint
applyVariations []   point = point
applyVariations vars point =
  mconcat $ map (\(Variation weight transform)-> 
    scalePoint weight $ transform point) 
                vars

-- helpers
pointAttrs point@(Point x y) =
  let r = pointRadius point
      theta = pointTheta point
      phi = pointPhi point
  in
    (x, y, r, theta, phi, point)
  
-- is there a way to just change point and repoint the dependent thunks in the record?
rePointVarP varp point =
  let (x, y, r, theta, phi, point) = pointAttrs point
  in
    varp {
        point = point
      , x = x
      , y = y
      , r = r
      , theta = theta
      , phi = phi
      } 

pointRadius (Point x y) = sqrt $ x^2 + y^2

-- did I get this right?  Should be the angle from the x axis.  opposite over adjacent...
pointTheta (Point x y) = atan2 y x

pointPhi (Point x y) = atan2 x y

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
linear (VarP {point}) = point

sinusoidal (VarP {point}) = fmap sin point

spherical (VarP {point, r}) =
  let sc = 1 / r^2
  in
    fmap (* sc) point

swirl (VarP {r, x, y}) =
  let r2 = r^2
      sr2 = sin r2
      cr2 = cos r2
  in
    Point (x * sr2 - y * cr2) (x * cr2 + y * sr2)

horseshoe (VarP {r, x, y}) =
  fmap (/ r) $ Point ((x - y) * (x + y)) (2*x*y)

polar (VarP {theta, r}) =
  Point (theta / pi) (r - 1)

handkerchief (VarP {theta, r}) =
    fmap (* r) $ Point (sin $ theta + r) (cos $ theta - r)

heart (VarP {theta, r}) =
  let tr = theta * r
  in
    fmap (* r) $ Point (sin tr) (- cos tr)

disc (VarP {theta, r}) =
  let top = theta / pi
      pr = pi * r
  in
    fmap (* top) $ Point (sin pr) (cos pr)

spiral (VarP {theta, r}) =
  let ct = cos theta
      st = sin theta
  in
    fmap (/ r) $ Point (ct + st) (st - ct)

hyperbolic (VarP {r, theta}) =
  Point (sin theta / r) (r * cos theta)

diamond (VarP {r, theta}) =
  Point (sin theta * cos r) (cos theta * sin r)

ex (VarP {r, theta}) =
  let p0 = sin $ theta + r
      p1 = cos $ theta - r
  in
    fmap (* r) $ Point (p0^3 + p1^3) (p0^3 - p1^3)

julia (VarP {omega1, r, theta}) =
  let a = theta / 2 + omega1
  in
    fmap (* sqrt r) $ Point (cos a) (sin a)


-- I wonder if the compiler will optimize out the unnecessary checks...
bent (VarP {point, x, y})
  | x >= 0 && y >= 0 = point
  | x <  0 && y >= 0 = Point (2 * x)  y
  | x >= 0 && y <  0 = Point      x  (y / 2)
  | x <  0 && y <  0 = Point (2 * x) (y / 2)
  
waves (VarP {b, c, e, f, x, y}) =
  Point (x + b * sin (y / c^2)) (y + e * sin (x / f^2))

fisheye varp@(VarP {x, y}) =
  eyefish (rePointVarP varp $ Point y x)


popcorn (VarP {x, y, c, f}) =
  Point (x + c * (sin . tan $ 3 * y)) (y + f * (sin . tan $ 3 * x))

exponential (VarP {x, y}) =
  fmap (* (exp $ x - 1)) $ Point (cos $ pi * y) (sin $ pi * y)

power (VarP {r, theta}) =
  fmap (* r**(sin theta)) $ Point (cos theta) (sin theta)

cosine (VarP {x, y}) =
  Point (cos (pi * x) * cosh y) (- 1 * sin (pi * x) * sinh y)

rings (VarP {c, r, theta}) =
  let m = (r + c^2) % (2 * c^2) - r * (1 - c^2)
  in
    fmap (* m) $ Point (cos theta) (sin theta)

fan (VarP {c, f, r, theta}) =
  let t = pi * c^2
      g z = fmap (* r) $ Point (cos z) (sin z)
  in
    if (theta + f) % t > t / 2 then
      g $ theta - t / 2
    else
      g $ theta + t / 2

eyefish (VarP {r, point}) =
  fmap (* (2 / (r + 1))) point

bubble (VarP {r, point}) =
  fmap (* (4 / (r^2 + 4))) point

cylinder (VarP {x, y})= Point (sin x) y

noise (VarP {psi1, psi2, x, y}) =
  let z = 2 * pi * psi2
  in
    fmap (* psi1) $ Point (x * cos z) (y * sin z)

blur (VarP {psi1, psi2}) =
  let z = 2 * pi * psi2
  in
    fmap (* psi1) $ Point (cos z) (sin z)

rings2 (VarP {vparams, r, theta}) =
  let [v] = getVps vparams ["rings2_val"]
      p = v^2
      t = r - 2 * p * trunc ((r + p) / (2 * p)) + r * (1 - p)
  in
    fmap (* t) $ Point (sin theta) (cos theta)

fan2 (VarP {vparams, r, theta}) =
  let [vx, vy] = getVps vparams ["fan2_x", "fan2_y"]
      p1 = pi * vx^2
      p2 = vy
      t = theta + p2 - p1 * trunc (2 * theta * p2 / p1)
      g z = fmap (* r) $ Point (sin z) (cos z)
  in
    if t > p1 / 2 then
      g $ theta - p1 / 2
    else
      g $ theta + p1 / 2

blob (VarP {vparams, r, theta}) =
  let [high, low, waves] = getVps vparams ["blob_high", "blob_low", "blob_waves"]
      m = r * (low + ((high - low) / 2) * (sin (waves * theta) + 1))
  in
    fmap (* m) $ Point (cos theta) (sin theta)

pdj (VarP {vparams, x, y}) =
  let [a,b,c,d] = getVps vparams ["pdj_a", "pdj_b", "pdj_c", "pdj_d"]
  in
    Point ((sin $ a * y) - (cos $ b * x)) ((sin $ c * x) - (cos $ d * y)) 

perspective (VarP {vparams, x, y}) =
  let [angle, dist] = getVps vparams ["perspective_angle", "perspective_dist"]
      m = dist / (dist - y * sin angle)
  in
    fmap (* m) $ Point x (y * cos angle)

julian (VarP {psi1, vparams, phi, r}) =
  let [power, dist] = getVps vparams ["julian_power", "julian_dist"]
      p = trunc $ (abs power) * psi1
      t = (phi + 2 * pi * p) / power
      m = r**(dist / power)
  in
    fmap (* m) $ Point (cos t) (sin t)

juliascope (VarP {psi1, lambda1, vparams, phi, r}) =
  let [power, dist] = getVps vparams ["juliascope_power", "juliascope_dist"]
      p = trunc $ (abs power) * psi1
      t = (lambda1 * phi + 2 * pi * p) / power
      m = r**(dist / power)
  in
    fmap (* m) $ Point (cos t) (sin t)

gaussian (VarP {psis}) =
  let (gauss, (psi5:_)) = gaussianR psis 
      z = 2 * pi * psi5
  in
    fmap (* gauss) $ Point (cos z) (sin z)

-- ask spot about 1) is ra the same as rA on the wiki? 2) why doesn't the code on the wiki divide the final X and Y by W?
-- following spec in the paper for now
radialblur (VarP {psis, vparams, weight, x, y, r, phi}) =
  let [angle] = getVps vparams ["radialblur_angle"]
      p = angle * pi / 2
      (gauss, _) = gaussianR psis
      t1 = weight * gauss
      t2 = phi + t1 * sin p
      t3 = t1 * cos p - 1
  in
    fmap (/ weight) $ Point (r * cos t2 + t3 * x) (r * sin t2 + t3 * y)

pie (VarP {psi1, psi2, psi3, vparams}) =
  let [slices, rotation, thickness] = getVps vparams ["pie_slices", "pie_rotation", "pie_thickness"]
      t1 = trunc $ psi1 * slices + 0.5
      t2 = rotation + 2 * pi / slices * (t1 + psi2 * thickness)
  in
    fmap (* psi3) $ Point (cos t2) (sin t2)

ngon (VarP {vparams, phi, r, point}) =
  let [power, sides, corners, circle] = getVps vparams ["ngon_power", "ngon_sides", "ngon_corners", "ngon_circle"]
      p = 2 * pi / sides
      t3 = phi - p * flr (phi / p)
      t4 = if t3 > p / 2 then t3 else t3 - p
      k = (corners * ((1 / cos t4) - 1) + circle) / r**power
  in
    fmap (* k) $ point  

curl (VarP {vparams, x, y}) =
  let [c1, c2] = getVps vparams ["curl_c1", "curl_c2"]
      t1 = 1 + c1 * x + c2 * (x^2 - y^2)
      t2 = c1 * y + 2 * c2 * x * y
      m = 1 / (t1^2 + t2^2)
  in
    fmap (* m) $ Point (x * t1 + y * t2) (y * t1 - x * t2)

rectangles (VarP {vparams, x, y}) =
  let [rx, ry] = getVps vparams ["rectangles_x", "rectangles_y"]
  in
    Point ((2 * flr (x / rx) + 1) * rx - x) ((2 * flr (y / ry) + 1) * ry - y)

arch (VarP {psi1, weight}) =
  let z = psi1 * pi * weight
  in
    Point (sin z) ((sin z)^2 / (cos z))

tangent (VarP {x, y}) =
  Point ((sin x) / (cos y)) ((sin y) / (cos x))

square (VarP {psi1, psi2, x, y}) =
  Point (psi1 - 0.5) (psi2 - 0.5)

rays (VarP {psi1, weight, r, x, y}) =
  let m = weight * (tan $ psi1 * pi * weight) / r^2
  in
    fmap (* m) $ Point (cos x) (sin y)

blade (VarP {psi1, weight, r, x}) =
  let z = psi1 * r * weight
  in
    fmap (* x) $ Point (cos z + sin z) (cos z - sin z)

secant2 (VarP {weight, r, x}) =
  let r' = weight * r
      t1 = cos r'
      t2 = 1 / t1
      y' = if t1 < 0 then
             t2 + 1
           else
             t2 - 1
  in
    Point x y'

twintrain (VarP {psis, weight, r, x}) =
  let (psi:_) = psis
      z = psi * r * weight
      t = (log $ (sin z)^2 + cos z) / (log 10)
  in
    fmap (* x) $ Point t (t - pi * sin z)

-- did following without wiki, FIX ORDER
cross (VarP {x, y, point}) =
  let m = sqrt $ 1 / (x^2 - y^2)^2
  in
    fmap (* m) point

-- are there additional variations on the wiki but not in the paper?

-- gotta be a better way to do this
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
  , ("arch", arch)
  , ("square", square)
  , ("rays", rays)
  , ("blade", blade)
  , ("secant2", secant2)
  , ("twintrain", twintrain)
  , ("cross", cross)
  ]
  
