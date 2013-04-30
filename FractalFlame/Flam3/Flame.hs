module FractalFlame.Flam3.Flame 
  ( postProcessFlame 
  , module FractalFlame.Flam3.Types.Flame
  )
where

import System.Random

import FractalFlame.Flam3.Types.Flame
import FractalFlame.Generator.Types.Generator
import FractalFlame.Palette
import FractalFlame.Symmetry

-- | Postprocessing steps to get a Flame ready for rendering after it has been parsed.
postProcessFlame :: StdGen -> Flame -> (Flame, StdGen)
postProcessFlame s = (addSymmetry s) . addPalette

-- | Convert a flame with a list of colors to a flame with a Palette
addPalette :: Flame -> Flame
addPalette flame@(Flame {colors=(ColorList colors')}) =
  let palette = ColorPalette $ buildPalette colors'
  in
    flame {colors = palette}
