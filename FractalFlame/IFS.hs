module IFS where

import IFSTypes

ifs iterationsToDiscard = do
  drop iterationsToDiscard . ifsHelper

ifsHelper rangeCheck 
          lastPoint 
          lastColorVal
          ((pre, post, colorVal):linearTriples) 
          variations 
          final 
          finalColorVal =
  let prePoint = maybe lastPoint ($ lastPoint) pre
      varsPoint = applyVariations variations prePoint
      postPoint = maybe varsPoint ($ varsPoint) post
      transColorVal = (lastColorVal + colorVal) / 2
      finalPoint = maybe postPoint ($ postPoint) final
      finalColorVal = (transColorVal + finalColorVal) / 2
  in
    let next = ifsHelper rangeCheck
                         finalPoint -- iterate regardless of range check
                         finalColorVal
                         linearTriples
                         variations
                         final
                         finalColorVal
    in
      if rangeCheck finalPoint then
        ((Plottable finalPoint finalColorVal):next)
      else
        next

