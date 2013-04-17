module FractalFlame.GLDisplay (
    displayLoop
  ) where
  
import Data.Array
import qualified Foreign
import GHC.Float
import Graphics.UI.GLUT
import System.Exit (exitWith, ExitCode(ExitSuccess))

import qualified FractalFlame.Histogram as H
import qualified FractalFlame.Color as C
import qualified FractalFlame.Types.PixelFlame as PF
import qualified FractalFlame.Types.Size as S

type Image = PixelData (Color3 GLfloat)

sizeRep :: PF.PixelFlame -> Size
sizeRep (PF.PixelFlame {size = (S.Size width height)}) =
  let w = fromIntegral width
      h = fromIntegral height
  in Size w h

-- convert a Pixel to a type OpenGL can display
pixelRep :: C.Color -> Color3 GLfloat
pixelRep (C.Color r g b a) = Color3 (realToFrac r) (realToFrac g) (realToFrac b)

makeImage :: PF.PixelFlame -> IO Image
makeImage pixelFlame = do
  fmap (PixelData RGB Float) . Foreign.newArray . map pixelRep $ PF.pixelFlame2Colors pixelFlame
  
display :: Size -> Image -> DisplayCallback
display size pixelData = do
  clear [ColorBuffer]
  -- resolve overloading, not needed in "real" programs
  let rasterPos2i = rasterPos :: Vertex2 GLint -> IO ()
  rasterPos2i (Vertex2 0 0)
  drawPixels size pixelData
  flush

reshape :: ReshapeCallback
reshape size@(Size w h) = do
  viewport $= (Position 0 0, size)
  matrixMode $= Projection
  loadIdentity
  ortho2D 0 (fromIntegral w) 0 (fromIntegral h)
  matrixMode $= Modelview 0
  loadIdentity

keyboard :: KeyboardMouseCallback
keyboard (Char '\27') Down _    _ = exitWith ExitSuccess
keyboard _            _    _    _ = return ()

myInit :: PF.PixelFlame -> IO (Size, Image)
myInit pixelFlame = do
  let size = sizeRep pixelFlame
  (progName, _args) <- getArgsAndInitialize
  initialDisplayMode $= [SingleBuffered, RGBMode]
  initialWindowSize $= size
  initialWindowPosition $= Position 100 100
  createWindow progName
  clearColor $= Color4 0 0 0 0
  shadeModel $= Flat
  rowAlignment Unpack $= 1
  image <- makeImage pixelFlame
  return (size, image)

displayLoop :: PF.PixelFlame -> IO ()
displayLoop pixelFlame = do
  (size, flameImage) <- myInit pixelFlame
  displayCallback $= display size flameImage
  reshapeCallback $= Just reshape
  keyboardMouseCallback $= Just keyboard
  mainLoop
