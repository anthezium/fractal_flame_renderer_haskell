module FractalFlame.IFSTypes where

import Data.Monoid

type Coord = Double
type FloatChannel = Double

data Color = Color {
    r :: FloatChannel
  , g :: FloatChannel
  , b :: FloatChannel
  , a :: FloatChannel
  }

instance Monoid Color where
  mempty = (Color 0 0 0 0)
  mappend (Color r1 g1 b1 a1) (Color r2 b2 g2 a2) = 
    (Color (r1 + r2) (g1 + g2) (b1 + b2) (a1 + a2))
    
scaleColor :: Color -> Color
scaleColor (Color r g b a) =
  let s = log a / a
      scale channel = s * channel 
  in
    (Color (scale r) (scale g) (scale b) (scale a))

gammaColor :: FloatChannel -> FloatChannel -> Color -> Color
gammaColor vibrancy gamma (Color r g b a) =
  let alphaGamma = a ** (1 / gamma)
      correct channel = vibrancy * alphaGamma * channel + (1 - vibrancy) * channel ** (1 / gamma)
  in
    (Color (correct r) (correct g) (correct b) (correct a))

type Palette = Coord -> Color -- should return Color with alpha channel 1.0

data Point = Point {
    x :: Coord
  , y :: Coord
  }

instance Monoid Point where
  mempty = (Point 0 0)
  mappend (Point x1 y1) (Point x2 y2) = (Point (x1 + x2) (y1 + y2))

type Transform = Point -> Point

data Variation = Variation {
    coefficient :: Coord
  , transform :: Transform
  }

scalePoint :: Coord -> Point -> Point
scalePoint coeff (Point x y) = (Point (x * coeff) (y * coeff)) 

{- 2-dimensional linear transformation matrix constants -}
data LinearParams = LinearParams {
    xx :: Coord
  , xy :: Coord
  , xc :: Coord
  , yx :: Coord
  , yy :: Coord
  , yc :: Coord
  }

{- base transforms (as opposed to variations) -}
data BaseTransform = BaseTransform {
    basePre  :: Maybe Transform -- transform before variations are run
  , basePost :: Maybe Transform -- transform after variations are run
  , baseColorVal :: Coord -- color associated with these transforms
  , baseWeight :: Coord -- likelihood of selection by IFS, value from 0 to 1
  }


data Plottable = Plottable {
    point :: Point
  , plottableColorVal :: Coord
  }
