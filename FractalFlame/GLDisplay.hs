module FractalFlame.GLDisplay (
    displayLoop
  ) where
  
import Data.Array
import Foreign(newArray)
import System.Exit (exitWith, ExitCode(ExitSuccess))
import Graphics.UI.GLUT

import FractalFlame.Flame

--data State = State { size :: IORef Size }
type Image = PixelData (Color3 GLubyte)

sizeRep :: PixelFlame -> Size
sizeRep pixelFlame =
  let w = fromIntegral $ flameWidth pixelFlame
      h = fromIntegral $ flameHeight pixelFlame
  in Size w h

-- convert a Pixel to a type OpenGL can display
--pixelRep :: Pixel -> (Color3 GLubyte)
pixelRep pixel = (Color3 (fromIntegral (r pixel)) (fromIntegral (g pixel)) (fromIntegral (b pixel)))

makeImage :: PixelFlame -> IO Image
makeImage flame =
  -- this is pretty inefficient... must be some way to go from my array to a "foreign" array
  fmap (PixelData RGB UnsignedByte) $ newArray [pixelRep pixel | pixel <- elems $ pixels flame]

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

myInit :: PixelFlame -> IO (Size, Image)
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

displayLoop :: PixelFlame -> IO ()
displayLoop pixelFlame = do
  (size, flameImage) <- myInit pixelFlame
  displayCallback $= display size flameImage
  reshapeCallback $= Just reshape
  keyboardMouseCallback $= Just keyboard
  mainLoop
