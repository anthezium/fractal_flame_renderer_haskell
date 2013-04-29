module FractalFlame.Flam3.Types.Flame where

import FractalFlame.Camera.Types.Camera
import FractalFlame.Flam3.Types.Xform
import FractalFlame.Flam3.Types.Color
import FractalFlame.Palette.Types.Palette
import FractalFlame.Types.Base
import FractalFlame.Types.Size

data FlameColors = ColorList [Color] | ColorPalette Palette

data Flame = Flame {
    colors     :: FlameColors
  , xforms     :: [Xform]
  , finalXform :: Maybe Xform
  , camera     :: Camera
  , symmetry   :: Int
  , gamma      :: Coord
  , vibrancy   :: Coord
  , quality    :: Int
  }

