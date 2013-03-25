module FractalFlame.IFSTypes where

import Data.Monoid
import Graphics.UI.GLUT (GLdouble)

import FractalFlame.Flame

type Coord = GLdouble 

data Point a = Point {
    x :: a
  , y :: a
  } deriving (Show)

instance Functor Point where
  fmap f (Point x y) = (Point (f x) (f y))

type CartesianPoint = Point Coord

-- seems like I should be able to generalize this to Monoid a => Monoid (Point a), 
-- but how to make the mempty/mappend definitions not recursive?
instance Num a => Monoid (Point a) where
  mempty = (Point 0 0) 
  mappend (Point x1 y1) (Point x2 y2) = (Point (x1 + x2) (y1 + y2))

type GridPoint = Point Int

data Size = Size {
    sizeWidth :: Int
  , sizeHeight :: Int
  }

type Transform = CartesianPoint -> CartesianPoint

data Variation = Variation {
    weight :: Coord
  , transform :: Transform
  }

scalePoint :: Num a => a -> Point a -> Point a
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
    plottablePoint :: CartesianPoint
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

