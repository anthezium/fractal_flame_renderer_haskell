module FractalFlame.Palette where

import Control.Arrow
import Data.Array.Unboxed (listArray, (!), Array)
import Data.Foldable
import Data.List
import Data.Monoid

import FractalFlame.Flame
import FractalFlame.IFSTypes

type Palette = Coord -> Color -- should return Color with alpha channel 1.0

buildPalette :: [Flam3Color] -> Palette
buildPalette colors =
  let scolors = sort colors
      mini = index $ head scolors
      maxi = index $ last scolors
      colorv = listArray (mini, maxi) $ map rgb scolors :: Array Int Color
      range = fromIntegral $ maxi - mini :: Coord
  in
    (\colorVal ->
      let dix = round $ range * colorVal
          ix = mini + dix
      in
        colorv ! ix)
