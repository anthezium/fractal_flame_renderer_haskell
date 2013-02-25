module FractalFlame.IFSTypes where

import Data.Monoid

import FractalFlame.Flame

type Coord = Double 

data Point = Point {
    x :: Coord
  , y :: Coord
  } deriving (Show)

instance Monoid Point where
  mempty = (Point 0 0)
  mappend (Point x1 y1) (Point x2 y2) = (Point (x1 + x2) (y1 + y2))

data GridPoint = GridPoint {
    px :: Int
  , py :: Int
  }

data Size = Size {
    sizeWidth :: Int
  , sizeHeight :: Int
  }

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
    plottablePoint :: Point
  , plottableColorVal :: Coord
  } deriving (Show)

data Flam3Flame = Flam3Flame {
    colors :: [Flam3Color]
  } deriving (Show)

data Flam3Color = Flam3Color {
    index :: Int
  , rgb :: Color
  } deriving (Show)

flam3RGBChannelMin = fromIntegral intChannelMin
flam3RGBChannelMax = fromIntegral intChannelMax

instance Eq Flam3Color where
  f1 == f2 = (index f1) == (index f2)

instance Ord Flam3Color where
  f1 `compare` f2 = (index f1) `compare` (index f2)

