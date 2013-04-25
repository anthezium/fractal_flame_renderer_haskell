module FractalFlame.Variation
  ( runVariation
  , applyVariations
  , module FractalFlame.Variation.Types.Variation
  , module FractalFlame.Variation.VTransforms
  )
where

import Control.Applicative
import Data.HashMap.Strict (HashMap, (!))
import Data.Monoid
import System.Random

import FractalFlame.Generator
import FractalFlame.Point.Types.CartesianPoint
import FractalFlame.Point.Types.Point
import FractalFlame.Types.Base
import FractalFlame.Types.LinearParams
import FractalFlame.Variation.Types.VParams
import FractalFlame.Variation.Types.VarP
import FractalFlame.Variation.Types.Variation
import FractalFlame.Variation.Util
import FractalFlame.Variation.VTransforms

-- exported
-- better way than typing all of this stuff?  template haskell?
-- | Apply a variation transformation to a CartesianPoint, returning the transformed point and a new seed.
runVariation :: Variation -- ^ 'variation'.  Supplies the VTransform (variation function itself), its weight and parameters corresponding to the current linear transformation.
             -> LinearParams -- ^ 'linearParams'.  Matrix coefficients for the current linear transformation.
             -> StdGen -- ^ 'seed'.  Current seed for generating random numbers (some variations consume random numbers).
             -> CartesianPoint -- ^ 'point'.  Point to which this variation should be applied.
             -> (CartesianPoint, StdGen) -- ^ (point', seed').  Transformed point and new seed.
runVariation (Variation weight vparams vtransform) linearParams@(LinearParams a b c d e f) seed point@(Point x y) = 
  let fList = concat . map (replicate 5) $ [psi, omega, lambda]
      (   [psi1, psi2, psi3, psi4, psi5, 
           omega1, omega2, omega3, omega4, omega5, 
           lambda1, lambda2, lambda3, lambda4, lambda5]
        , gSeed) = generatorSequence fList seed
      gList = replicate 4 psi
      (gaussians, seed') = generatorSequence gList gSeed
      gaussianR = gaussianRandom gaussians
      (x, y, r, theta, phi) = pointAttrs point
      -- is there a way to avoid typing each name twice?
      point' = vtransform $ VarP {
           psi1 = psi1
        ,  psi2 = psi2
        ,  psi3 = psi3
        ,  psi4 = psi4
        ,  psi5 = psi5
        ,  omega1 = omega1
        ,  omega2 = omega2
        ,  omega3 = omega3
        ,  omega4 = omega4
        ,  omega5 = omega5
        ,  lambda1 = lambda1
        ,  lambda2 = lambda2
        ,  lambda3 = lambda3
        ,  lambda4 = lambda4
        ,  lambda5 = lambda5
        ,  gaussianR = gaussianR
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
    in
      (point', seed')

applyVariations :: LinearParams -> [Variation] -> StdGen -> CartesianPoint -> (CartesianPoint, StdGen)
applyVariations _            []   seed point = (point, seed)
applyVariations linearParams vars seed point =
  let (seed', seed'') = split seed
      seedVars  = zip (seeds seed') vars
      totalWeight = sum $ map FractalFlame.Variation.Types.Variation.weight vars -- to normalize weights
      point' = mconcat $ map (\(s, variation@(Variation weight _ _))-> 
                               let transform = runVariation variation linearParams s
                               in
                                 scalePoint (weight / totalWeight) . fst . transform $ point) 
                             seedVars
  in
    (point', seed'')

