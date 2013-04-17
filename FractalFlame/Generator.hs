module FractalFlame.Generator
( module FractalFlame.Generator
, Generator(..)
) where

import Control.Monad.State
import Data.List
import System.Random
import Test.QuickCheck
import Test.QuickCheck.Gen

import FractalFlame.Flam3.Types.Xform
import FractalFlame.Generator.Types.Generator
import FractalFlame.Point.Types.CartesianPoint
import FractalFlame.Point.Types.Point
import FractalFlame.Types.Base

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
