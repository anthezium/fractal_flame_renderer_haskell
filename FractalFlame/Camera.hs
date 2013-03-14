module FractalFlame.Camera (
    Camera(..)
  , project
  , inCameraCheck
  ) where

import FractalFlame.IFSTypes

data Camera = Camera {
    cameraSize :: Size
  , cameraCenter :: CartesianPoint
  , cameraScale :: Coord
  , cameraRotate :: Coord
  , cameraZoom :: Coord
  }

-- transform a point in the IFS coordinate system to a
-- point in the output image grid
project :: Camera -> CartesianPoint -> GridPoint
project camera@(Camera size@(Size width height) 
                       center@(Point cx cy)
                       scale
                       rotate
                       zoom)
        point@(Point x y) =
  let (wshift, hshift, dx, dy) = cameraDimensions camera
      (Point x' y') = rotateBy center rotate point
      px = round ((x' - cx) / dx * wshift + wshift) :: Int
      py = round ((y' - cy) / dy * hshift + hshift) :: Int
  in 
    Point px py

-- is this point inside the viewport(?)?
inCameraCheck :: Camera -> CartesianPoint -> Bool
inCameraCheck camera@(Camera size@(Size width height)
                             center@(Point cx cy)
                             scale
                             rotate
                             zoom)
              (Point x y) =
  let (_, _, dx, dy) = cameraDimensions camera
      rot = rotateBy center rotate
      rx = dx + cx
      lx = cx - dx
      ty = dy + cy
      by = cy - dy
      tr' = rot $ Point rx ty
      br' = rot $ Point rx by
      bl' = rot $ Point lx by
      tl' = rot $ Point lx ty
      -- top tl'x = tl'y
      top = lineFunc tl' tr'
      -- right tr'y = tr'x
      right = invLineFunc tr' br'
      -- bottom bl'x = bl'y
      bottom = lineFunc bl' br'
      -- left tl'y = tl'x
      left = invLineFunc tl' bl'
  in
       bottom x <= y && y <= top x
    && left y <= x && x <= right y

-- helpers

lineFunc :: Floating a => Point a -> Point a -> a -> a
lineFunc p1@(Point x1 y1)
         p2@(Point x2 y2) 
         x =
  -- TODO(ted): I could compute this stuff and just return a function
  -- that explicitly embedded the precomputed values,
  -- but I'm sure there's a way to make the compiler do that
  -- when I curry.  Ditto for lots of other places where I curry.  I should look
  -- up how that works, whether or not it's automatic.                                                    
  let m = (y2 - y1) / (x2 - x1)
      b = y1 - (x1 * m)
  in
    m * x + b

-- TODO(ted): include a test that verifies that invLineFunc is the inverse of lineFunc
invLineFunc :: Floating a => Point a -> Point a -> (a -> a)
invLineFunc p1@(Point x1 y1)
            p2@(Point x2 y2) =
  lineFunc (Point y1 x1) (Point y2 x2)

-- distance from center to right and top edges of viewport 
-- in pixels and in IFS coordinate system
cameraDimensions :: Camera -> (Coord, Coord, Coord, Coord)
cameraDimensions (Camera size@(Size width height)
                         center@(Point cx cy)
                         scale
                         rotate
                         zoom) =
  let wshift = fromIntegral (div width 2) :: Coord
      hshift = fromIntegral (div height 2) :: Coord
      dx = wshift / scale / zoom
      dy = hshift / scale / zoom
  in
    (wshift, hshift, dx, dy)  

-- jacked from flam3.c
rotateBy :: Floating a => Point a -> a -> Point a -> Point a
rotateBy (Point cx cy) rotate (Point x y) =
      -- find vector relative to center
  let dx = x - cx
      dy = y - cy
      -- convert degrees to radians
      theta = rotate / 360 * 2 * pi
      c = cos theta
      s = sin theta
      -- rotate vector relative to center
      rx = c * dx - s * dy
      ry = s * dx + c * dy
      -- new point is rotated vector + center
      x' = cx + rx
      y' = cy + ry
  in
    Point x' y'
