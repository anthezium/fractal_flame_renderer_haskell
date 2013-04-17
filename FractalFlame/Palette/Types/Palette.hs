module FractalFlame.Palette.Types.Palette where

import FractalFlame.Types.Base
import FractalFlame.Color.Types.Color

type Palette = Coord -> Color -- should return Color with alpha channel 1.0

