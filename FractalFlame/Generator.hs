module Generator where

import IFSTypes

import Data.List
import System.Random
import Test.QuickCheck
import Test.QuickCheck.Gen

infiniteSample :: StdGen -> [a] -> [a]
infiniteSample s items =
  let n = length items
      gen = mapM (\_ -> choose(0,n-1)) [1..]
  in 
    map (items !!) $ unGen gen s 1

--need to add a function that uses weights to meet the paper spec
weightedInfiniteSample :: StdGen -> [(Coord, a)] -> [a]
weightedInfiniteSample s freqItems =
  let n = length freqItems
      -- no need to normalize, frequency does it for us.  multiply to get some resolution before rounding.
      freqGens = map (\(freq, item) -> ((round $ freq * 10000), return item)) freqItems
      gen = mapM (\_ -> frequency freqGens) [1..]
  in
    unGen gen s 1

-- infinite list that samples a list of BaseTransforms based on their weights
sampleBaseTransforms :: StdGen -> [BaseTransform] -> [BaseTransform]
sampleBaseTransforms s = weightedInfiniteSample s . map (\xform -> (baseWeight xform, xform))
