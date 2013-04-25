module FractalFlame.Generator
  ( module FractalFlame.Generator
  , module FractalFlame.Generator.Types.Generator
  ) 
where

import Control.Monad.State
import Data.Bits
import Data.List
import Data.Random.Normal
import System.Random
import Test.QuickCheck.Gen

import FractalFlame.Flam3.Types.Xform
import FractalFlame.Generator.Types.Generator
import FractalFlame.Point.Types.CartesianPoint
import FractalFlame.Point.Types.Point
import FractalFlame.Types.Base

-- | Generate a random value in the subrange of a Gaussian distribution:
--   [mean - stdev * clipDevs, mean + stdev * clipDevs]
boundedGaussian pt@(mean, stdev, clipDevs) s =
  let res@(v, s') = normal' (mean, stdev) s
      min = mean - clipDevs * stdev
      max = mean + clipDevs * stdev
  in
    -- sample until we find a value within clipDevs standard deviations of the mean
    -- DANGER: runtime could grow large for a relatively flat distribution and small clipDevs
    if min <= v && v <= max then res else boundedGaussian pt s'

-- | Given an ordered list, superimpose a bell curve over it with the mean in the middle of the list and choose an item
--   with probability proportional to the height of the curve at that item's position in the list.
gaussianChoose :: (RealFrac a, Floating a, Random a, Ord a) => (a, a, a) -> [b] -> Generator b
gaussianChoose pt@(mean, stdev, clipDevs) vs s =
  let (i, s') = boundedGaussian (mean, stdev, clipDevs) s -- mean 0, stdev 1.  let's clip it @ 4 standard deviations
      halfRange = stdev * clipDevs
      gRange = 2 * halfRange
      i' = i + halfRange
      i'' = i' / gRange
      ix = (round $ i'' * (fromIntegral (length vs))) - 1
      v = vs !! ix
  in
    (v, s')
      
-- | Generate a random flame symmetry parameter based on a Gaussian distribution
symmetryGenGaussian :: Generator Int
symmetryGenGaussian s =
  let -- first, specify a nicely shaped Gaussian bell curve
      mean = 0 :: Coord -- why isn't some floating-point type inferred? because there are multiple concrete types that satisfy the class constraints?
      stdev = 1 :: Coord
      clipDevs = 4 :: Coord
      -- next, set up a set of symmetry values from which to choose.  items in the middle are most likely to be chosen,
      -- items at either end least likely
      -- symmetry  none   |  reflective, rotational |  rotational |  none (extra 1 so -1 and 2 are equally likely)
      syms =       [1, 1] ++ [-25..(-1)]              ++ [2..25]    ++ [1, 1, 1]
  in 
    gaussianChoose (mean, stdev, clipDevs) syms s

-- | Generate a random flame symmetry parameter with behavior equivalent to Scott Draves' flam3-render
symmetryGenDraves :: Generator Int
symmetryGenDraves s =
  let baseDistribution =
        [ (-4), (-3)
        , (-2), (-2), (-2)
        , (-1), (-1), (-1)
        ,   2 ,   2 ,   2 
        ,   3 ,   3 
        ,   4 ,   4   
        ]
      baseLookup ix = baseDistribution !! (ix `mod` (length baseDistribution))
      -- idk why, but he chooses a fresh random number if r1 is odd
      ([r1, r2, r3], s') = generatorSequence (replicate 3 $ randomR (minBound, maxBound)) s :: ([Int], StdGen)
      sym = if (r1 .&. 1) /= 0 then -- half the time (8/16 probability total)
              baseLookup r2 -- choose one of baseDistribution
            else if (r2 .&. 31) /= 0 then -- if not base, 1/8 of the time (1/16 probability total)
              r3 `mod` 13 - 6 -- -6 to 6 inclusive
            else -- if not base, 7/8 of the time (7/16 probability total)
              r3 `mod` 51 - 25 -- -25 to 25 inclusive
  in
    (sym, s')  

loopGen :: Gen a -> Generator a
loopGen g = (\s ->
  let (s', s'') = split s
  in
    (unGen g s' 1, s''))

loopSampler :: [a] -> Generator a
loopSampler items =
  let gens = map return items
  in
    loopGen $ oneof gens

weightedLoopSampler :: [(Coord, a)] -> Generator a
weightedLoopSampler freqItems =
      -- no need to normalize, frequency does it for us.  multiply to get some resolution before rounding.
  let freqGens = map (\(freq, item) -> ((round $ freq * 10000), return item)) freqItems
  in
    loopGen $ frequency freqGens

-- returns a function that samples a list of BaseTransforms based on their weights
xformSampler :: [Xform] -> Generator Xform
xformSampler = weightedLoopSampler . map (\xform@(Xform {weight}) -> (weight, xform))

-- random variables for initialization
genFirstPoint :: Generator CartesianPoint
genFirstPoint s =
  let (x, s')  = randomR (-1, 1) s  :: (Coord, StdGen) -- why won't it infer this?
      (y, s'') = randomR (-1, 1) s' :: (Coord, StdGen)
  in
    (Point x y, s'')

genFirstColorIx :: Generator Coord
genFirstColorIx = randomR (0, 1)

-- | infinite list of new seeds
seeds :: StdGen -> [StdGen]
seeds s =
  let (s', s'') = split s
  in
    (s':seeds s'')
    -- would (s':seeds s') also be correct?

-- | random number [0,pi]
psi :: Generator Coord
psi = randomR (0, pi)

-- | random number 0 or pi
omega :: Generator Coord
omega = loopGen $ oneof [return 0, return pi]

-- | random number -1 or 1
lambda :: Generator Coord
lambda = loopGen $ oneof [return (-1), return 1]

-- | get the results for a list of Generators sequentially, passing the seed returned by each Generator to the next
generatorSequence :: [Generator a] -> StdGen -> ([a], StdGen)
generatorSequence fs seed = (flip runState) seed $ do
  results <- mapM runGen fs
  return results
  where runGen f = do
          seed' <- get
          let (res, seed'') = f seed'
          put seed''
          return res
