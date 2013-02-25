module FractalFlame.ParseFlam3 where

import Text.RegexPR
import Text.XML.HXT.Core

import FractalFlame.Flame
import FractalFlame.IFSTypes

parseRGBAttr :: String -> Color
parseRGBAttr s = 
  let [r,g,b] = parse s
  in 
    Color (normalizeChannel r) (normalizeChannel g) (normalizeChannel b) 1
  where parse = map read . concat . ggetbrsRegexPR "\\d+"
        normalizeChannel channel = (channel - flam3RGBChannelMin) / (flam3RGBChannelMax - flam3RGBChannelMin)

-- adapted from example at http://www.haskell.org/haskellwiki/HXT/Practical/Simple2
parseXML file = readDocument [ withValidate no
                             , withRemoveWS yes  -- throw away formating WS
                             ] file
                                                           
atTag tag = deep (isElem >>> hasName tag)

getColors = atTag "color" >>>
  proc c -> do
    index <- getAttrValue $ "index" -< c
    rgb <- getAttrValue $ "rgb" -< c
    returnA -< Flam3Color {
        index = read index
      , rgb = parseRGBAttr rgb
      }

parseFlam3 :: String -> IO Flam3Flame
parseFlam3 flam3 = do
  colors <- runX (parseXML flam3 >>> getColors)
  return Flam3Flame { colors = colors }
