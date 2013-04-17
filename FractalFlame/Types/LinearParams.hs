module FractalFlame.Types.LinearParams where

import FractalFlame.Types.Base

{- 2-dimensional linear transformation matrix constants -}
data LinearParams = LinearParams {
    a :: Coord
  , b :: Coord
  , c :: Coord
  , d :: Coord
  , e :: Coord
  , f :: Coord
  }

