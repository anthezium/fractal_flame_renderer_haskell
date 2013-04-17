module FractalFlame.Palette
( buildPalette
, Palette
) where

import Control.Arrow
import Data.Array.Unboxed (listArray, (!), Array)
import Data.Foldable
import Data.List
import Data.Monoid

import qualified FractalFlame.Flam3.Types.Color as F3C
import FractalFlame.Color
import FractalFlame.Palette.Types.Palette
import FractalFlame.Types.Base

buildPalette :: [F3C.Color] -> Palette
buildPalette colors =
  let scolors = sort colors
      mini = F3C.index $ head scolors
      maxi = F3C.index $ last scolors
      colorv = listArray (mini, maxi) $ map F3C.rgb scolors :: Array Int Color
      range = fromIntegral $ maxi - mini :: Coord
  in
    (\colorIx ->
      let dix = round $ range * colorIx
          ix = mini + dix
      in
        colorv ! ix)
