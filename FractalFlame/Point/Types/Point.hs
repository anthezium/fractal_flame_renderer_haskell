{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverlappingInstances #-}

module FractalFlame.Point.Types.Point where

import Data.Monoid

import FractalFlame.Types.Base

data Point a = Point {
    x :: a
  , y :: a
  } deriving (Show)

instance Functor Point where
  fmap f (Point x y) = (Point (f x) (f y))

instance Monoid a => Monoid (Point a) where
  mempty = (Point mempty mempty) 
  mappend (Point x1 y1) (Point x2 y2) = (Point (x1 `mappend` x2) (y1 `mappend` y2))

instance Monoid (Point Coord) where
  mempty = Point 0 0
  (Point x1 y1) `mappend` (Point x2 y2) = Point (x1 + x2) (y1 + y2)
  
-- | Multiply a Point's x and y values by the same coefficient.
scalePoint :: Num a => a -> Point a -> Point a
scalePoint coeff = fmap (* coeff)

