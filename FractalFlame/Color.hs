module FractalFlame.Color
( scaleColor
, gammaColor
, module FractalFlame.Color.Types.Color
) where

import FractalFlame.Color.Types.Color

scaleColor :: FloatChannel -> Color -> Color
scaleColor amax (Color r g b a) =
      -- learn more about "functional dispatch" as listed here: http://www.haskell.org/haskellwiki/Currying
      -- are expressions that only depend on supplied parameters evaluated and memoized? (e.g. log amax)
  let brightness = log a / log amax
      aScale = brightness / a
      scale channel = channel * aScale
  in
    Color (scale r) (scale g) (scale b) brightness

-- | Apply gamma correction to a Color.
gammaColor :: FloatChannel -- ^ 'vibrancy'. A floating-point value [0,1] that determines how independently the channels in a given pixel are gamma corrected.  If 1, all channels are corrected by the same coefficient, if 0 all channels are corrected independently, if in between, blends global and independent coefficients linearly.
           -> FloatChannel -- ^ 'gamma'.  A positive floating-point value.  As it grows, channels dim exponentially.
           -> Color -- ^ 'color'.  Color to correct.
           -> Color
gammaColor vibrancy gamma color@(Color r g b a) = 
  let applyGamma channel = channel ** (1 / gamma)
      alphaGamma = applyGamma a
      -- weigh alpha-based and channel-based gamma corrections by vibrancy, more vibrancy == more alpha contribution
      correct channel = channel * (vibrancy * (alphaGamma / a) + (1 - vibrancy) * applyGamma channel)
  in
    Color (correct r) (correct g) (correct b) (correct a)

