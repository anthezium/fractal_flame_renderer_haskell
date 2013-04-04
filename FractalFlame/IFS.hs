module FractalFlame.IFS where

import FractalFlame.IFSTypes
import FractalFlame.LinearTransformation
import FractalFlame.Variation

-- an infinite list of points and corresponding color values in the 
-- specified IFS (Iterated Function System)
ifs :: Int
    -> (CartesianPoint -> Bool)
    -> CartesianPoint
    -> Coord
    -> [BaseTransform]
    -> [Generator]
    -> [Variation]
    -> Maybe Transform
    -> Maybe Coord
    -> [Plottable]
ifs iterationsToDiscard 
    rangeCheck 
    firstPoint 
    firstColorVal
    baseTransforms 
    generators
    variations 
    final 
    finalColorVal = 
  let plottableCheck = rangeCheck . plottablePoint
      -- strictly evaluate lastPoint and lastColorVal since they only depend on preceding list items,
      -- for which we have no reason to keep thunks. (we need thunks for the subsequent items because
      -- the list is infinite)
      helper !lastPoint 
             !lastColorVal
             (xform:baseTransforms')
             generators = 
        let preParams = basePreParams xform
            postParams = basePostParams xform
            [pre, post] = map (>>= Just . linearTransformation) [preParams, postParams]
            colorVal = baseColorVal xform
            -- apply first affine transformation if specified
            prePoint = maybe lastPoint ($ lastPoint) pre
            -- apply variations if variations and a pre-transformation were specified
            -- (should this work without a pre-transformation if no variations requiring linear parameters are specified?)
            (generators', varsPoint) = maybe (generators, prePoint) (\linearParams -> applyVariations generators linearParams variations prePoint) preParams
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
                          generators'
        in
          (this:next)
  in
    drop iterationsToDiscard . 
    filter plottableCheck    $ helper firstPoint 
                                      firstColorVal
                                      baseTransforms 
                                      generators
