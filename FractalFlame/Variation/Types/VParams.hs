module FractalFlame.Variation.Types.VParams 
  ( VParams
  , (!)
  , empty
  , fromList
  , toList
  ) 
where

import Data.HashMap.Strict

import FractalFlame.Types.Base

-- | Map of names to floating-point values for parameterized variation functions
type VParams = HashMap String Coord
