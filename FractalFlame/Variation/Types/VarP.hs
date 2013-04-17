module FractalFlame.Variation.Types.VarP where

import FractalFlame.Point.Types.CartesianPoint
import FractalFlame.Types.Base
import FractalFlame.Types.LinearParams
import FractalFlame.Variation.Types.VParams

-- should I worry about the number of fields w.r.t. efficiency?  will building all those thunks take extra time or use a significant amount of extra memory?  VarPs are only bound in the scope of runVariation, so they should be garbage collected when it returns.  This should happen automatically since each new point is eagerly evaluated and that should evaluate all the thunks that are needed and let the current VarP go out of scope.
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

