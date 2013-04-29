module FractalFlame.IFS
( ifs )
where

import System.Random

import FractalFlame.Flam3.Types.Xform
import FractalFlame.Generator
import FractalFlame.LinearTransformation
import FractalFlame.Palette
import FractalFlame.Types.Plottable
import FractalFlame.Types.Transform
import FractalFlame.Point.Types.CartesianPoint
import FractalFlame.Types.Base
import FractalFlame.Variation

-- | an infinite list of points and corresponding color values in the 
-- | specified IFS (Iterated Function System)
ifs :: Int                      -- ^ number of initial iterations for which no Plottable should be returned
    -> (CartesianPoint -> Bool) -- ^ function to check whether a given point is within the viewport
    -> CartesianPoint           -- ^ starting point for the system
    -> Coord                    -- ^ starting color index for the system
    -> StdGen                   -- ^ starting seed
    -> Generator Xform          -- ^ sampling function to get the next transformation
    -> Maybe Xform              -- ^ optional final transformation
    -> [Plottable]
ifs iterationsToDiscard 
    rangeCheck 
    firstPoint 
    firstColorIx
    firstSeed
    getXform
    finalXform = 
  let plottableCheck = rangeCheck . point
      -- strictly evaluate lastPoint and lastColorIx since they only depend on preceding list items,
      -- for which we have no reason to keep thunks. (we need thunks for the subsequent items because
      -- the list is infinite)
      helper !lastPoint 
             !lastColorIx
             seed = 
        let (xform, seed') = getXform seed
            -- apply affine pre and post transformations, associated variations if specified
            xformRet@((xformPoint, xformColorIx), seed'') = applyXform xform lastPoint lastColorIx seed'
            -- apply final transformation if specified
            ((thisPoint, thisColorIx), seed''')  = maybe xformRet (\xf -> applyXform xf xformPoint xformColorIx seed'') finalXform
            this = Plottable thisPoint thisColorIx
            next = helper thisPoint 
                          thisColorIx
                          seed'''
        in
          (this:next)
  in
    drop iterationsToDiscard . 
    filter plottableCheck    $ helper firstPoint 
                                      firstColorIx
                                      firstSeed
             
-- helpers

applyXform :: Xform -> CartesianPoint -> Coord -> Generator (CartesianPoint, Coord)
applyXform !(Xform {preParams, postParams, colorIx, variations}) !lastPoint !lastColorIx seed =
  let [pre, post] = map (>>= Just . linearTransformation) [preParams, postParams]
      -- apply first affine transformation if specified
      prePoint = maybe lastPoint ($ lastPoint) pre
      -- apply variations if variations and a pre-transformation were specified
      -- (should this work without a pre-transformation if no variations requiring linear parameters are specified?)
      (varsPoint, seed') = maybe (prePoint, seed) (\linearParams -> applyVariations linearParams variations seed prePoint) preParams
      -- apply affine post-transformation if specified
      thisPoint = maybe varsPoint ($ varsPoint) post
      -- blend with new color index if specified
      thisColorIx = maybe lastColorIx (updateColorIx lastColorIx) colorIx
  in
    ((thisPoint, thisColorIx), seed')

updateColorIx :: Coord -> (Coord -> Coord)
updateColorIx curColorIx = (/ 2) . (+ curColorIx)
