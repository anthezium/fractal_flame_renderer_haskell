module FractalFlame.Symmetry 
( addSymmetry ) 
where

import Data.Maybe

import FractalFlame.Flam3.Types.Flame
import FractalFlame.Flam3.Types.Xform
import FractalFlame.Types.LinearParams

-- | Add rotational and/or dihedral symmetry transforms to the specified Flame based on the symmetry parameter:
--   sym=2 or more means rotational
--   sym=1 means identity, ie no symmetry
--   sym=0 means pick a random symmetry (maybe none)
--   sym=-1 means bilateral (reflection)
--   sym=-2 or less means rotational and reflective
addSymmetry :: Flame 
            -> Flame
addSymmetry flame@(Flame {xforms, symmetry}) =
  let totalWeight = sum $ map weight xforms
      flipX = if symmetry < 0 then Just $ flipXform totalWeight else Nothing
      fabsym = fromIntegral $ abs symmetry
      theta = 2 * pi / fabsym
      thetas = map (* theta) [1..fabsym - 1]
      rotateXs = map (rotateXform totalWeight) thetas
      symXs = (maybeToList flipX) ++ rotateXs
  in
    flame { xforms = xforms ++ symXs }

-- helpers

simpleXform weight = Xform {
    preParams = Nothing
  , postParams = Nothing
  , colorIx = Nothing
  , weight = 0
  , symmetry = 0
  , variations = []
  }


flipXform weight = (simpleXform weight) {
    preParams = Just $ LinearParams (-1) 0 0 0 1 0
  }

rotateXform weight theta = 
  let c = cos theta
      s = sin theta
  in
    (simpleXform weight) {
      preParams = Just $ LinearParams c (- s) 0 s c 0 
    }

