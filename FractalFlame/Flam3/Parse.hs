module FractalFlame.Flam3.Parse where

import Text.RegexPR
import Text.XML.HXT.Core

import qualified FractalFlame.Flam3.Types.Color as F3C
import qualified FractalFlame.Flam3.Types.Flame as F3F
import FractalFlame.Color.Types.Color
import FractalFlame.Types.PixelFlame

parseRGBAttr :: String -> Color
parseRGBAttr s = 
  let [r,g,b] = parse s
  in 
    Color (normalizeChannel r) (normalizeChannel g) (normalizeChannel b) 1
  where parse = map read . concat . ggetbrsRegexPR "\\d+"
        normalizeChannel channel = (fromIntegral (channel - F3C.rgbChannelMin)) / (fromIntegral (F3C.rgbChannelMax - F3C.rgbChannelMin))

-- adapted from example at http://www.haskell.org/haskellwiki/HXT/Practical/Simple2
parseXML file = readDocument [ withValidate no
                             , withRemoveWS yes  -- throw away formating WS
                             ] file
                                                           
atTag tag = deep (isElem >>> hasName tag)

{-
getFlame = atTag "flame" >>>
  proc c -> do
    

getXforms = 
  let requiredNames = ["weight", "color", "symmetry"]
      optionalNames = ["coeffs", "post"]
      variationNames = map fst variations -- or keys from variationMap
      -- make a version of this that can make Maybes for optional names.  use that function that filters out Nothings?
      -- look up api to see how to get a Maybe for optional params
      -- * arrows will output an empty list when they fail.  simply leave lists that may have multiple items as lists, convert singleton or empty lists into maybes after arrow processing
      buildNameMap = HMS.fromList $ map (\name -> (, (getAttrValue $ name -< c)))
  in
    atTag "xform" >>> proc c -> do
      required <- buildNameMap requiredNames
      optional <- buildOptNameMap optionalNames
      variations <- buildOptNameMap variationNames --filter out Nothings since we're just making a list
      -- look up api to see how to get a list of attr names in the tag.  then i need to filter out standard ones, build list of variations then filter them out, then put everything else in a vparams hash.  probably should redo stuff above to operate on lists of names.
        -- if I can figure out how to do this with arrows, processAttrl will do arrow processing on the element's list of attributes. documentation at http://hackage.haskell.org/packages/archive/hxt/8.5.2/doc/html/Text-XML-HXT-Arrow-XmlArrow.html
      -- there must be some way (w/Template Haskell?) to assign to record fields based on names in a statically defined list
-}

getColors = atTag "color" >>>
  proc c -> do
    index <- getAttrValue $ "index" -< c
    rgb <- getAttrValue $ "rgb" -< c
    returnA -< F3C.Color {
        index = read index
      , rgb = parseRGBAttr rgb
      }

parseFlam3 :: String -> IO F3F.Flame
parseFlam3 flam3 = do
  colors <- runX (parseXML flam3 >>> getColors)
  return F3F.Flame { colors = colors }
