module FractalFlame.Generator where

import Data.List
import System.Random
import Test.QuickCheck
import Test.QuickCheck.Gen

import FractalFlame.IFSTypes

infiniteListGen :: StdGen -> Gen a -> [a]
infiniteListGen s g =
  let gen = mapM (\_ -> g) [1..]
  in
    unGen gen s 1

infiniteSample :: StdGen -> [a] -> [a]
infiniteSample s items = 
  let gens = map (\x -> return x) items
  in 
    infiniteListGen s $ oneof gens

--need to add a function that uses weights to meet the paper spec
weightedInfiniteSample :: StdGen -> [(Coord, a)] -> [a]
weightedInfiniteSample s freqItems =
  let n = length freqItems
      -- no need to normalize, frequency does it for us.  multiply to get some resolution before rounding.
      freqGens = map (\(freq, item) -> ((round $ freq * 10000), return item)) freqItems
  in infiniteListGen s $ frequency freqGens

-- infinite list that samples a list of BaseTransforms based on their weights
sampleBaseTransforms :: StdGen -> [BaseTransform] -> [BaseTransform]
sampleBaseTransforms s = weightedInfiniteSample s . map (\xform -> (baseWeight xform, xform))

-- random variables for use in variations

-- random number [0,pi]
psi :: StdGen -> [Coord]
psi s = 
  let (x, s') = randomR (0, pi) s
  in
    (x:(psi s'))

-- random number 0 or pi
omega :: StdGen -> [Coord]
omega s = infiniteListGen s $ oneof [return 0, return pi]

-- random number -1 or 1
lambda :: StdGen -> [Coord]
lambda s = infiniteListGen s $ oneof [return (-1), return 1]
