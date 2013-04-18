{-# LANGUAGE DisambiguateRecordFields, NamedFieldPuns, TemplateHaskell #-}

module FractalFlame.Variation.VTransforms where

import qualified Data.HashMap.Strict as HMS
import Language.Haskell.Extract

import FractalFlame.Point.Types.Point
import FractalFlame.Types.Base
import FractalFlame.Variation.Types.VarP
import FractalFlame.Variation.Util

-- Variations
vt_linear (VarP {point}) = point

vt_sinusoidal (VarP {point}) = fmap sin point

vt_spherical (VarP {point, r}) =
  let sc = 1 / r^2
  in
    fmap (* sc) point

vt_swirl (VarP {r, x, y}) =
  let r2 = r^2
      sr2 = sin r2
      cr2 = cos r2
  in
    Point (x * sr2 - y * cr2) (x * cr2 + y * sr2)

vt_horseshoe (VarP {r, x, y}) =
  fmap (/ r) $ Point ((x - y) * (x + y)) (2*x*y)

vt_polar (VarP {theta, r}) =
  Point (theta / pi) (r - 1)

vt_handkerchief (VarP {theta, r}) =
    fmap (* r) $ Point (sin $ theta + r) (cos $ theta - r)

vt_heart (VarP {theta, r}) =
  let tr = theta * r
  in
    fmap (* r) $ Point (sin tr) (- cos tr)

vt_disc (VarP {theta, r}) =
  let top = theta / pi
      pr = pi * r
  in
    fmap (* top) $ Point (sin pr) (cos pr)

vt_spiral (VarP {theta, r}) =
  let ct = cos theta
      st = sin theta
  in
    fmap (/ r) $ Point (ct + st) (st - ct)

vt_hyperbolic (VarP {r, theta}) =
  Point (sin theta / r) (r * cos theta)

vt_diamond (VarP {r, theta}) =
  Point (sin theta * cos r) (cos theta * sin r)

vt_ex (VarP {r, theta}) =
  let p0 = sin $ theta + r
      p1 = cos $ theta - r
  in
    fmap (* r) $ Point (p0^3 + p1^3) (p0^3 - p1^3)

vt_julia (VarP {omega1, r, theta}) =
  let a = theta / 2 + omega1
  in
    fmap (* sqrt r) $ Point (cos a) (sin a)


-- I wonder if the compiler will optimize out the unnecessary checks...
vt_bent (VarP {point, x, y})
  | x >= 0 && y >= 0 = point
  | x <  0 && y >= 0 = Point (2 * x)  y
  | x >= 0 && y <  0 = Point      x  (y / 2)
  | x <  0 && y <  0 = Point (2 * x) (y / 2)
  
vt_waves (VarP {b, c, e, f, x, y}) =
  Point (x + b * sin (y / c^2)) (y + e * sin (x / f^2))

vt_fisheye varp@(VarP {x, y}) =
  vt_eyefish (rePointVarP varp $ Point y x)


vt_popcorn (VarP {x, y, c, f}) =
  Point (x + c * (sin . tan $ 3 * y)) (y + f * (sin . tan $ 3 * x))

vt_exponential (VarP {x, y}) =
  fmap (* (exp $ x - 1)) $ Point (cos $ pi * y) (sin $ pi * y)

vt_power (VarP {r, theta}) =
  fmap (* r**(sin theta)) $ Point (cos theta) (sin theta)

vt_cosine (VarP {x, y}) =
  Point (cos (pi * x) * cosh y) (- 1 * sin (pi * x) * sinh y)

vt_rings (VarP {c, r, theta}) =
  let m = (r + c^2) % (2 * c^2) - r * (1 - c^2)
  in
    fmap (* m) $ Point (cos theta) (sin theta)

vt_fan (VarP {c, f, r, theta}) =
  let t = pi * c^2
      g z = fmap (* r) $ Point (cos z) (sin z)
  in
    if (theta + f) % t > t / 2 then
      g $ theta - t / 2
    else
      g $ theta + t / 2

vt_eyefish (VarP {r, point}) =
  fmap (* (2 / (r + 1))) point

vt_bubble (VarP {r, point}) =
  fmap (* (4 / (r^2 + 4))) point

vt_cylinder (VarP {x, y})= Point (sin x) y

vt_noise (VarP {psi1, psi2, x, y}) =
  let z = 2 * pi * psi2
  in
    fmap (* psi1) $ Point (x * cos z) (y * sin z)

vt_blur (VarP {psi1, psi2}) =
  let z = 2 * pi * psi2
  in
    fmap (* psi1) $ Point (cos z) (sin z)

vt_rings2 (VarP {vparams, r, theta}) =
  let [v] = getVps vparams "rings2" ["val"]
      p = v^2
      t = r - 2 * p * trunc ((r + p) / (2 * p)) + r * (1 - p)
  in
    fmap (* t) $ Point (sin theta) (cos theta)

vt_fan2 (VarP {vparams, r, theta}) =
  let [vx, vy] = getVps vparams "fan2" ["x", "y"]
      p1 = pi * vx^2
      p2 = vy
      t = theta + p2 - p1 * trunc (2 * theta * p2 / p1)
      g z = fmap (* r) $ Point (sin z) (cos z)
  in
    if t > p1 / 2 then
      g $ theta - p1 / 2
    else
      g $ theta + p1 / 2

vt_blob (VarP {vparams, r, theta}) =
  let [high, low, waves] = getVps vparams "blob" ["high", "low", "waves"]
      m = r * (low + ((high - low) / 2) * (sin (waves * theta) + 1))
  in
    fmap (* m) $ Point (cos theta) (sin theta)

vt_pdj (VarP {vparams, x, y}) =
  let [a,b,c,d] = getVps vparams "pdj" ["a", "b", "c", "d"]
  in
    Point ((sin $ a * y) - (cos $ b * x)) ((sin $ c * x) - (cos $ d * y)) 

vt_perspective (VarP {vparams, x, y}) =
  let [angle, dist] = getVps vparams "perspective" ["angle", "dist"]
      m = dist / (dist - y * sin angle)
  in
    fmap (* m) $ Point x (y * cos angle)

vt_julian (VarP {psi1, vparams, phi, r}) =
  let [power, dist] = getVps vparams "julian" ["power", "dist"]
      p = trunc $ (abs power) * psi1
      t = (phi + 2 * pi * p) / power
      m = r**(dist / power)
  in
    fmap (* m) $ Point (cos t) (sin t)

vt_juliascope (VarP {psi1, lambda1, vparams, phi, r}) =
  let [power, dist] = getVps vparams "juliascope" ["power", "dist"]
      p = trunc $ (abs power) * psi1
      t = (lambda1 * phi + 2 * pi * p) / power
      m = r**(dist / power)
  in
    fmap (* m) $ Point (cos t) (sin t)

vt_gaussian (VarP {psi1, gaussianR}) =
  let z = 2 * pi * psi1
  in
    fmap (* gaussianR) $ Point (cos z) (sin z)

-- ask spot about 1) is ra the same as rA on the wiki? 2) why doesn't the code on the wiki divide the final X and Y by W?
-- following spec in the paper for now
vt_radialblur (VarP {gaussianR, vparams, weight, x, y, r, phi}) =
  let [angle] = getVps vparams "radialblur" ["angle"]
      p = angle * pi / 2
      t1 = weight * gaussianR
      t2 = phi + t1 * sin p
      t3 = t1 * cos p - 1
  in
    fmap (/ weight) $ Point (r * cos t2 + t3 * x) (r * sin t2 + t3 * y)

vt_pie (VarP {psi1, psi2, psi3, vparams}) =
  let [slices, rotation, thickness] = getVps vparams "pie" ["slices", "rotation", "thickness"]
      t1 = trunc $ psi1 * slices + 0.5
      t2 = rotation + 2 * pi / slices * (t1 + psi2 * thickness)
  in
    fmap (* psi3) $ Point (cos t2) (sin t2)

vt_ngon (VarP {vparams, phi, r, point}) =
  let [power, sides, corners, circle] = getVps vparams "ngon" ["power", "sides", "corners", "circle"]
      p = 2 * pi / sides
      t3 = phi - p * flr (phi / p)
      t4 = if t3 > p / 2 then t3 else t3 - p
      k = (corners * ((1 / cos t4) - 1) + circle) / r**power
  in
    fmap (* k) $ point  

vt_curl (VarP {vparams, x, y}) =
  let [c1, c2] = getVps vparams "curl" ["c1", "c2"]
      t1 = 1 + c1 * x + c2 * (x^2 - y^2)
      t2 = c1 * y + 2 * c2 * x * y
      m = 1 / (t1^2 + t2^2)
  in
    fmap (* m) $ Point (x * t1 + y * t2) (y * t1 - x * t2)

vt_rectangles (VarP {vparams, x, y}) =
  let [rx, ry] = getVps vparams "rectangles" ["x", "y"]
  in
    Point ((2 * flr (x / rx) + 1) * rx - x) ((2 * flr (y / ry) + 1) * ry - y)

vt_arch (VarP {psi1, weight}) =
  let z = psi1 * pi * weight
  in
    Point (sin z) ((sin z)^2 / (cos z))

vt_tangent (VarP {x, y}) =
  Point ((sin x) / (cos y)) ((sin y) / (cos x))

vt_square (VarP {psi1, psi2, x, y}) =
  Point (psi1 - 0.5) (psi2 - 0.5)

vt_rays (VarP {psi1, weight, r, x, y}) =
  let m = weight * (tan $ psi1 * pi * weight) / r^2
  in
    fmap (* m) $ Point (cos x) (sin y)

vt_blade (VarP {psi1, weight, r, x}) =
  let z = psi1 * r * weight
  in
    fmap (* x) $ Point (cos z + sin z) (cos z - sin z)

vt_secant2 (VarP {weight, r, x}) =
  let r' = weight * r
      t1 = cos r'
      t2 = 1 / t1
      y' = if t1 < 0 then
             t2 + 1
           else
             t2 - 1
  in
    Point x y'

vt_twintrain (VarP {psi1, weight, r, x}) =
  let z = psi1 * r * weight
      t = (log $ (sin z)^2 + cos z) / (log 10)
  in
    fmap (* x) $ Point t (t - pi * sin z)

vt_cross (VarP {x, y, point}) =
  let m = sqrt $ 1 / (x^2 - y^2)^2
  in
    fmap (* m) point

vt_disc2 (VarP {vparams, x, y, phi}) =
  let [rot, twist] = getVps vparams "disc2" ["rot", "twist"]
      rtp = rot * pi
      k = case twist of 
            twist | twist > 2 * pi     -> 1 + twist - 2 * pi
                  | twist < (- 2) * pi -> 1 + twist + 2 * pi
                  | otherwise          -> 1
      st = k * (sin twist)
      ct = k * (cos twist)
      t = rtp * (x + y)
      srt = sin t
      crt = cos t
      m = phi / pi
  in    
    fmap (* m) $ Point (srt + ct) (crt + st)

vt_supershape (VarP {vparams, r, theta, psi1, point}) =
  let [m, n1, n2, n3, holes, rnd] = getVps vparams "supershape" ["m", "n1", "n2", "n3", "holes", "md"]
      pm_4 = m / 4
      pneg1_n1 = (-1.0) / n1
      th = pm_4 * theta + pi / 4
      t1 = (**n2) . abs . sin $ th
      t2 = (**n3) . abs . cos $ th 
      mx = (rnd * psi1 + (1 - rnd) * r - holes) * (t1 + t2)**pneg1_n1 / r
  in
    fmap (* mx) point

vt_flower (VarP {vparams, theta, psi1}) =
  let [holes, petals] = getVps vparams "flower" ["holes", "petals"]
      m = (psi1 - holes) * (cos $ petals * theta)
  in
    fmap (* m) $ Point (cos theta) (sin theta)

vt_conic (VarP {vparams, theta, psi1}) =
  let [holes, eccen] = getVps vparams "conic" ["holes", "petals"]
      m = (psi1 - holes) * eccen / (1 + eccen * (cos theta))
  in
    fmap (* m) $ Point (cos theta) (sin theta)

vt_parabola (VarP {vparams, r, psi1, psi2}) =
  let [height, width] = getVps vparams "parabola" ["height", "width"]
  in
    Point (height * (sin r)^2 * psi1) (width * (cos r) * psi2)

vt_bent2 (VarP {vparams, x, y}) =
  let [vx, vy] = getVps vparams "bent2" ["x", "y"]
      x' = if x < 0 then x * vx else x
      y' = if y < 0 then y * vy else y
  in
    Point x' y'

vTransforms = map (\(name, f) -> (drop 3 name, f)) $(functionExtractor "^vt_")

vTransformNames = map fst vTransforms

vTransformByName = HMS.fromList vTransforms

