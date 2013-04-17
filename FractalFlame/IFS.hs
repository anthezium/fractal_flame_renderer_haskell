module FractalFlame.IFS where

import System.Random

import FractalFlame.Flam3.Types.Xform
import FractalFlame.Generator
import FractalFlame.LinearTransformation
import FractalFlame.Palette
import FractalFlame.Types.Plottable
import FractalFlame.Types.Transform
import FractalFlame.Point.Types.CartesianPoint
import FractalFlame.Types.Base
import FractalFlame.Variation

-- | an infinite list of points and corresponding color values in the 
-- | specified IFS (Iterated Function System)
ifs :: Int
    -> (CartesianPoint -> Bool)
    -> CartesianPoint
    -> Coord
    -> StdGen
    -> Generator Xform
    -> [Variation]
    -> Maybe Transform
    -> Maybe Coord
    -> [Plottable]
ifs iterationsToDiscard 
    rangeCheck 
    firstPoint 
    firstColorIx
    firstSeed
    getXform
    variations 
    final 
    finalColorIx = 
  let plottableCheck = rangeCheck . point
      -- strictly evaluate lastPoint and lastColorIx since they only depend on preceding list items,
      -- for which we have no reason to keep thunks. (we need thunks for the subsequent items because
      -- the list is infinite)
      helper !lastPoint 
             !lastColorIx
             seed = 
        let ((Xform {preParams, postParams, colorIx}), seed') = getXform seed
            [pre, post] = map (>>= Just . linearTransformation) [preParams, postParams]
            -- apply first affine transformation if specified
            prePoint = maybe lastPoint ($ lastPoint) pre
            -- apply variations if variations and a pre-transformation were specified
            -- (should this work without a pre-transformation if no variations requiring linear parameters are specified?)
            (varsPoint, seed'') = maybe (prePoint, seed') (\linearParams -> applyVariations linearParams variations seed' prePoint) preParams
            -- apply affine post-transformation if specified
            postPoint = maybe varsPoint ($ varsPoint) post
            -- blend last and new color indices
            transColorIx = (lastColorIx + colorIx) / 2
            -- apply final affine transformation if specified
            thisPoint = maybe postPoint ($ postPoint) final
            -- blend with final color index if specified
            thisColorIx = maybe transColorIx (\cv -> (transColorIx + cv) / 2) finalColorIx
            this = Plottable thisPoint thisColorIx
            next = helper thisPoint 
                          thisColorIx
                          seed''
        in
          (this:next)
  in
    drop iterationsToDiscard . 
    filter plottableCheck    $ helper firstPoint 
                                      firstColorIx
                                      firstSeed
                                      
