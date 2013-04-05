module FractalFlame.IFSTypes where

import Data.HashMap.Strict (HashMap)
import Data.Monoid
import Graphics.UI.GLUT (GLdouble)
import System.Random

import FractalFlame.Flame

type Coord = GLdouble 

data Point a = Point {
    pointX :: a
  , pointY :: a
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
type Generator a = StdGen -> (a, StdGen)

-- should I worry about the number of fields w.r.t. efficiency?  will building all those thunks take extra time or use a significant amount of extra memory?  VarPs are only bound in the scope of runVariation, so they should be garbage collected when it returns.  This should happen automatically since each new point is eagerly evaluated and that should evaluate all the thunks that are needed and let the current VarP go out of scope.
type VParams = HashMap String Coord

data VarP = VarP {
    psi1         :: Coord
  , psi2         :: Coord
  , psi3         :: Coord
  , psi4         :: Coord
  , psi5         :: Coord
  , omega1       :: Coord
  , omega2       :: Coord
  , omega3       :: Coord
  , omega4       :: Coord
  , omega5       :: Coord
  , lambda1      :: Coord
  , lambda2      :: Coord
  , lambda3      :: Coord
  , lambda4      :: Coord
  , lambda5      :: Coord
  , gaussianR    :: Coord
  , linearParams :: LinearParams
  , a            :: Coord
  , b            :: Coord
  , c            :: Coord
  , d            :: Coord
  , e            :: Coord
  , f            :: Coord
  , vparams      :: VParams
  , weight       :: Coord
  , x            :: Coord
  , y            :: Coord
  , r            :: Coord
  , theta        :: Coord
  , phi          :: Coord
  , point        :: CartesianPoint
  }

type VTransform = VarP -> CartesianPoint

data Variation = Variation {
    variationWeight :: Coord
  , variationVParams :: VParams
  , variationTransform :: VTransform
  }

scalePoint :: Num a => a -> Point a -> Point a
scalePoint coeff = fmap (* coeff)

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
    basePreParams  :: Maybe LinearParams -- params for transform before variations are run
  , basePostParams :: Maybe LinearParams -- params for transform after variations are run
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

