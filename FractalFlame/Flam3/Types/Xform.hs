module FractalFlame.Flam3.Types.Xform where

import FractalFlame.Types.Base
import FractalFlame.Types.LinearParams
import FractalFlame.Variation.Types.Variation
import FractalFlame.Variation.Types.VParams

-- | Basic unit sampled by Iterated Function System.  Corresponds to an xform element in a flam3 file.
data Xform = Xform {
    preParams  :: Maybe LinearParams -- ^ params for transform before variations are run
  , postParams :: Maybe LinearParams -- ^ params for transform after variations are run
  , colorIx    :: Maybe Coord        -- ^ color index associated with these transforms.  Nothing means keep old colorIx
  , weight     :: Coord              -- ^ likelihood of selection by IFS, value from 0 to 1
  , symmetry   :: Coord
  , variations :: [Variation]        -- ^ weighted set of variations
  }

