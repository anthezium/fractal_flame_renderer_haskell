module FractalFlame.IFS where

import FractalFlame.IFSTypes
import FractalFlame.Variation

-- an infinite list of points and corresponding color values in the 
-- specified IFS (Iterated Function System)
ifs :: Int
    -> (CartesianPoint -> Bool)
    -> CartesianPoint
    -> Coord
    -> [BaseTransform]
    -> [Variation]
    -> Maybe Transform
    -> Maybe Coord
    -> [Plottable]
ifs iterationsToDiscard 
    rangeCheck 
    firstPoint 
    firstColorVal
    baseTransforms 
    variations 
    final 
    finalColorVal = 
  let plottableCheck = rangeCheck . plottablePoint
      -- strictly evaluate lastPoint and lastColorVal since they only depend on preceding list items,
      -- for which we have no reason to keep thunks. (we need thunks for the subsequent items because
      -- the list is infinite)
      helper !lastPoint 
             !lastColorVal
             (xform:baseTransforms') = 
        let pre = basePre xform
            post = basePost xform
            colorVal = baseColorVal xform
            -- apply first affine transformation if specified
            prePoint = maybe lastPoint ($ lastPoint) pre
            -- apply variations
            varsPoint = applyVariations variations prePoint
            -- apply affine post-transformation if specified
            postPoint = maybe varsPoint ($ varsPoint) post
            -- blend last and new color indices
            transColorVal = (lastColorVal + colorVal) / 2
            -- apply final affine transformation if specified
            thisPoint = maybe postPoint ($ postPoint) final
            -- blend with final color index if specified
            thisColorVal = maybe transColorVal (\cv -> (transColorVal + cv) / 2) finalColorVal
            this = Plottable thisPoint thisColorVal
            next = helper thisPoint 
                          thisColorVal
                          baseTransforms'
        in
          (this:next)
  in
    drop iterationsToDiscard . 
    filter plottableCheck    $ helper firstPoint 
                                      firstColorVal
                                      baseTransforms 
