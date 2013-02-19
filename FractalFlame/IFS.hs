module FractalFlame.IFS where

import FractalFlame.IFSTypes
import FractalFlame.Variation

ifs :: Int
    -> (Point -> Bool)
    -> Point
    -> Coord
    -> [BaseTransform]
    -> [Variation]
    -> Maybe Transform
    -> Maybe Coord
    -> [Plottable]
ifs iterationsToDiscard 
    rangeCheck 
    lastPoint 
    lastColorVal
    (xform:baseTransforms) 
    variations 
    final 
    finalColorVal = do
  drop iterationsToDiscard $ ifsHelper rangeCheck 
                                       lastPoint 
                                       lastColorVal
                                       (xform:baseTransforms) 
                                       variations 
                                       final 
                                       finalColorVal
                                             
ifsHelper :: (Point -> Bool)                 
          -> Point                           
          -> Coord                           
          -> [BaseTransform]                 
          -> [Variation]
          -> Maybe Transform
          -> Maybe Coord
          -> [Plottable]
ifsHelper rangeCheck 
          lastPoint 
          lastColorVal
          (xform:baseTransforms) 
          variations 
          final 
          finalColorVal =
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
      nextPoint = maybe postPoint ($ postPoint) final
      -- blend with final color index if specified
      nextColorVal = maybe transColorVal (\cv -> (transColorVal + cv) / 2) finalColorVal
  in
    let next = ifsHelper rangeCheck
                         nextPoint -- iterate regardless of range check
                         nextColorVal
                         baseTransforms
                         variations
                         final
                         finalColorVal
    in
      if rangeCheck nextPoint then
        ((Plottable nextPoint nextColorVal):next)
      else
        next

