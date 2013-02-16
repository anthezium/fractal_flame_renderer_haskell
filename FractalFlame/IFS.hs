module IFS where

import IFSTypes
import Variation

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
      prePoint = maybe lastPoint ($ lastPoint) pre
      varsPoint = applyVariations variations prePoint
      postPoint = maybe varsPoint ($ varsPoint) post
      transColorVal = (lastColorVal + colorVal) / 2
      finalPoint = maybe postPoint ($ postPoint) final
      fcv = maybe transColorVal (\cv -> (transColorVal + cv) / 2) finalColorVal
  in
    let next = ifsHelper rangeCheck
                         finalPoint -- iterate regardless of range check
                         fcv
                         baseTransforms
                         variations
                         final
                         finalColorVal
    in
      if rangeCheck finalPoint then
        ((Plottable finalPoint fcv):next)
      else
        next

