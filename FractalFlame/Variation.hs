module Variation where

import Data.Monoid

import IFSTypes

applyVariations :: [Variation] -> Point -> Point
applyVariations []   point = point
applyVariations vars point =
  mconcat $ map (\(Variation coeff transform)-> 
    scalePoint coeff $ transform point) 
                vars
      
