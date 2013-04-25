{-# LANGUAGE Arrows #-}

module FractalFlame.Flam3.Parse
  ( parseFlam3 ) 
where

import qualified Data.HashMap.Strict as HMS
import Data.Maybe
import Text.RegexPR
import Text.XML.HXT.Core

import FractalFlame.Color.Types.Color
import qualified FractalFlame.Flam3.Types.Color as F3C
import qualified FractalFlame.Flam3.Types.Flame as F3F
import qualified FractalFlame.Flam3.Types.Xform as F3X
import FractalFlame.Types.LinearParams
import FractalFlame.Types.PixelFlame
import qualified FractalFlame.Variation.Types.Variation as V
import qualified FractalFlame.Variation.VTransforms as VT

-- * Public Interface

-- | Parse a .flam3 XML file.
parseFlam3 :: String -- ^ Path to .flam3
           -> IO F3F.Flame
parseFlam3 flam3 = do
  flames <- runX (parseXML flam3 >>> getFlame)
  -- should only be one flame element per file
  return $ head flames 
  
-- * XMLTree transformer Arrows

-- | Arrow to transform flame elements in an XMLTree (in flam3 files, flame is the root element) to Flames
getFlame = atTag "flame" >>> proc c -> do
  colors <- listA getColors -< c
  xforms <- listA getXforms -< c
  returnA -< F3F.Flame { 
                 colors = colors
               , xforms = xforms
               }

-- | Arrow to transform xform elements in an XMLTree to Xforms
getXforms = 
  atTag "xform" >>> proc c -> do
    weight <- getAttrValue "weight" -< c
    color <- getAttrValue "color" -< c
    symmetry <- getAttrValue "symmetry" -< c
    coeffs <- getAttrValue "coeffs" -< c
    post <- listA (hasName "post" >>> getAttrValue "post") -< c
    variations <- listA 
                    (hasNameIn VT.vTransformNames 
                     >>> 
                     attrPair) -< c
    vparams <- listA 
                 (hasNamePrefixIn (map (++ "_") VT.vTransformNames) 
                  >>>
                  attrPair 
                  >>> 
                  -- convert strings to VParams value type
                  second (arr read)) -< c
    returnA -< let vparams' = HMS.fromList vparams
               in
                 F3X.Xform {
                     weight = read weight
                   , colorIx = read color
                   , symmetry = read symmetry
                   , preParams = Just . parseCoeffs $ coeffs
                   , postParams = (listToMaybe post) >>= (Just . parseCoeffs)
                   , variations = map (\(name, vWeight) -> 
                                      let vt = VT.vTransformByName HMS.! name
                                      in
                                        V.Variation {
                                            vTransform = vt
                                          , weight = read vWeight
                                          , vParams = vparams'
                                          })
                                    variations
                   }

-- | Arrow to transform color elements in an XMLTree to Colors
getColors = atTag "color" >>>
  proc c -> do
    index <- getAttrValue $ "index" -< c
    rgb <- getAttrValue $ "rgb" -< c
    returnA -< F3C.Color {
        index = read index
      , rgb = parseRGBAttr rgb
      }

-- * Helpers

-- adapted from example at http://www.haskell.org/haskellwiki/HXT/Practical/Simple2
parseXML file = readDocument [ withValidate no
                             , withRemoveWS yes  -- throw away formating WS
                             ] file
                                                           
atTag tag = deep (isElem >>> hasName tag)

hasNameIn = foldl1 (<+>) . map hasName

hasNamePrefixIn = foldl1 (<+>) . map hasNamePrefix

-- | Arrow to convert an attr and its value to a pair 
-- > (attr_name, attr_val)
attrPair = proc c -> do
  name <- getName -< c
  val <- getAttrValue name -<< c
  returnA -< (name, val)

-- * Attribute text parsers

-- | Parse color rgb attribute string, convert to a Color
parseRGBAttr :: String -> Color
parseRGBAttr s = 
  let [r,g,b] = parse s
  in 
    Color (normalizeChannel r) (normalizeChannel g) (normalizeChannel b) 1
  where parse = map read . concat . ggetbrsRegexPR "\\d+"
        normalizeChannel channel = (fromIntegral (channel - F3C.rgbChannelMin)) / (fromIntegral (F3C.rgbChannelMax - F3C.rgbChannelMin))

-- | Parse xform/finalxform coeffs/post attribute string, convert to LinearParams
parseCoeffs :: String -> LinearParams
parseCoeffs s =
  let [a,b,c,d,e,f] = parse s
  in
    LinearParams a b c d e f
  where parse = map read . concat . ggetbrsRegexPR "[0-9.]+"
