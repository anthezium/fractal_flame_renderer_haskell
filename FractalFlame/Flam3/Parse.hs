{-# LANGUAGE Arrows, DisambiguateRecordFields, NamedFieldPuns #-}

module FractalFlame.Flam3.Parse
  ( parseFlam3 ) 
where

import Data.Maybe
import Text.RegexPR
import Text.XML.HXT.Core

import FractalFlame.Camera.Types.Camera
import FractalFlame.Color.Types.Color
import qualified FractalFlame.Flam3.Types.Color as F3C
import qualified FractalFlame.Flam3.Types.Flame as F3F
import qualified FractalFlame.Flam3.Types.Xform as F3X
import FractalFlame.Point.Types.CartesianPoint
import FractalFlame.Point.Types.Point
import FractalFlame.Types.LinearParams
import FractalFlame.Types.PixelFlame
import FractalFlame.Types.Size
import qualified FractalFlame.Variation.Types.Variation as V
import qualified FractalFlame.Variation.Types.VParams as VP
import qualified FractalFlame.Variation.VTransforms as VT

-- * Public Interface

-- | Parse a .flam3 XML file.
parseFlam3 :: String -- ^ Path to .flam3
           -> IO F3F.Flame
parseFlam3 flam3 = do
  -- should only be one flame element per file
  [flame] <- runX (parseXML flam3 >>> getFlame)
  return flame 
  
-- * XMLTree transformer Arrows

-- | Arrow to transform flame elements in an XMLTree (in flam3 files, flame is the root element) to Flames
--getFlame :: ArrowXml a => a XmlTree F3F.Flame
getFlame = atTag "flame" >>> proc c -> do
  colors <- listA getColors -< c
  xforms <- listA getXforms -< c
  finalxform <- listA (hasName "finalxform" >>> getFinalXform) -< c
  symmetry <- getSymmetry -< c
  camera <- getCamera -< c
  gamma <- getAttrValue "gamma" -< c
  vibrancy <- getAttrValue "vibrancy" -< c
  quality <- getAttrValue "quality" -< c
  returnA -< F3F.Flame { 
                 colors = F3F.ColorList colors
               , xforms = xforms
               , finalXform = listToMaybe finalxform
               , symmetry = symmetry
               , camera = camera 
               , gamma = read gamma
               , vibrancy = read vibrancy
               , quality = read quality
               }

-- | Arrow to transform camera attributes of flame elements in an XMLTree to Cameras
--getCamera :: ArrowXml a => a XmlTree Camera
getCamera = proc c -> do
  size <- getAttrValue "size" -< c
  center <- getAttrValue "center" -< c
  scale <- getAttrValue "scale" -< c
  rotate <- getAttrValue "rotate" -< c
  zoom <- getAttrValue "zoom" -< c
  returnA -< Camera {
                 size = parseSize size
               , center = parseCenter center
               , scale = read scale
               , rotate = read rotate
               , zoom = read zoom
               }

-- | Arrow to transform a symmetry element in an XMLTree to an integer symmetry kind
--getSymmetry :: ArrowXml a => a XmlTree Int
getSymmetry =
  atTag "symmetry" >>> proc c -> do
    kind <- getAttrValue "kind" -< c
    returnA -< read kind

-- | Arrow to transform xform elements in an XMLTree to Xforms
--getXforms :: ArrowXml a => a XmlTree F3X.Xform
getXforms = 
  atTag "xform" >>> getGeneralXform

-- | Arrow to transform finalxform elements in an XMLTree to Xforms
--getFinalXform :: ArrowXml a => a XmlTree F3X.Xform
getFinalXform =
  atTag "finalxform" >>> getGeneralXform
  
-- | Arrow to convert any element in an xmltree with xform attributes to an Xform
--getGeneralXform :: ArrowXml a => a XmlTree F3X.Xform
getGeneralXform = proc c -> do
  weight <- getAttrValue "weight" -< c
  color <- getAttrValue "color" -< c
  symmetry <- getAttrValue "symmetry" -< c
  coeffs <- getAttrValue "coeffs" -< c
  post <- listA (hasName "post" >>> getAttrValue "post") -< c
  variations <- listA 
                  (getAttrl
                   >>>
                   hasNameIn VT.vTransformNames 
                   >>> 
                   attrPair) -< c
  vparams <- listA 
               (getAttrl
                >>>
                hasNameMatchIn (map (++ "_") VT.vTransformNames) 
                >>>
                attrPair 
                >>> 
                -- convert strings to VParams value type
                second (arr read)) -< c
  returnA -< let vparams' = VP.fromList vparams
             in
               F3X.Xform {
                   weight = read weight
                 , colorIx = Just $ read color
                 , symmetry = read symmetry
                 , preParams = Just . parseCoeffs $ coeffs
                 , postParams = (listToMaybe post) >>= (Just . parseCoeffs)
                 , variations = map (\(name, vWeight) -> 
                                      let vt = VT.vTransformByName VT.! name
                                      in
                                        V.Variation {
                                            name = name
                                          , weight = read vWeight
                                          , vParams = vparams'
                                          , vTransform = vt
                                          })
                                  variations
                 }

-- | Arrow to transform color elements in an XMLTree to Colors
--getColors :: ArrowXml a => a XmlTree F3C.Color
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
--parseXML :: ArrowXml a => String -> a XmlTree XmlTree
parseXML file = readDocument [ withValidate no
                             , withRemoveWS yes  -- throw away formating WS
                             ] file
                                                           
-- | Given a name, return an arrow to convert an XmlTree to outermost subtrees rooted at elements with that name
--atTag :: ArrowXml a => String -> a XmlTree XmlTree
atTag tag = deep (isElem >>> hasName tag)

-- | Given a list of names, return an arrow to filter for elements whose names are in the list
--hasNameIn :: ArrowXml a => [String] -> a XmlTree XmlTree
hasNameIn = foldl1 (<+>) . map hasName

-- | Given a list of regular expressions, return an arrow to filter for elements that match one or more in the list 
--hasNameMatchIn :: ArrowXml a => [String] -> a XmlTree XmlTree
hasNameMatchIn = foldl1 (<+>) . map hasNameMatch

-- | Given a regular expressions, return an arrow to filter for elements whose names match it
--hasNameMatch :: ArrowXml a => String -> a XmlTree XmlTree
hasNameMatch rgx = hasSNameWith $ isJust . (matchRegexPR rgx)

-- | Given a predicate, return an arrow to filter for elements that match it
--hasSNameWith :: ArrowXml a => (String -> Bool) -> a XmlTree XmlTree
hasSNameWith p = (getName >>> isA p) `guards` this

-- | Arrow to convert an attr and its value to a pair 
-- > (attr_name, attr_val)
attrPair = getName &&& (getChildren >>> getText)

-- * Attribute text parsers

-- | Parse color rgb attribute string, convert to a Color
parseRGBAttr :: String -> Color
parseRGBAttr s = 
  let [r,g,b] = parseInts s
  in 
    Color (normalizeChannel r) (normalizeChannel g) (normalizeChannel b) 1
  where normalizeChannel channel = (fromIntegral (channel - F3C.rgbChannelMin)) / (fromIntegral (F3C.rgbChannelMax - F3C.rgbChannelMin))

-- | Parse xform/finalxform coeffs/post attribute string, convert to LinearParams
parseCoeffs :: String -> LinearParams
parseCoeffs s =
  let [a,b,c,d,e,f] = parseFloats s
  in
    LinearParams a b c d e f

parseSize :: String -> Size
parseSize s =
  let [w, h] = parseInts s
  in
    Size w h

parseCenter :: String -> CartesianPoint
parseCenter s =
  let [x, y] = parseFloats s
  in
    Point x y

parseFloats :: (Read a, Floating a) => String -> [a]
parseFloats = parseWhitespaceList "[0-9.\\-]+"

parseInts :: (Read a, Integral a) => String -> [a]
parseInts = parseWhitespaceList "[0-9\\-]+"

parseWhitespaceList :: Read a => String -> String -> [a]
parseWhitespaceList regex = map read . concat . (ggetbrsRegexPR regex) 
