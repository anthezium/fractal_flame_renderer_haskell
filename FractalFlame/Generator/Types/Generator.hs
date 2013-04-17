module FractalFlame.Generator.Types.Generator where

import System.Random

type Generator a = StdGen -> (a, StdGen)
