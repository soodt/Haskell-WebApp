module DrawingDSL ( renderDrawing, Drawing, exampleAllShapes, exampleAllTransformations, exampleAllCompositions) where

import Data.Word (Word8)
import Codec.Picture (generateImage, Image, PixelRGB8(..))
import Data.List (sortBy)
import Data.Function (on)

-- Colour Datatype
data Colour = RGB Word8 Word8 Word8 deriving (Show, Eq)

-- Shape Datatype
data Shape
  = Circle Float            -- Radius of the circle
  | Rectangle Float Float   -- Width and Height of the Rectangle
  | Ellipse Float Float     -- Radius X and Radius Y of the Ellipse
  | Polygon [(Float, Float)] -- List of vertices (convex polygon in clockwise order)
  deriving (Show, Eq)

-- Transform Datatype
data Transform
  = Scale Float Float       -- Scale X, Y
  | Rotate Float            -- Rotation angle in degrees
  | Translate Float Float   -- Translate by (x, y)
  | Shear Float Float       -- Shear X, Y
  deriving (Show, Eq)

-- Drawing datatype
data Drawing
  = ShapeWithColor Shape Colour
  | Transformed Transform Drawing
  | Composite Drawing Composition Drawing
  deriving (Show, Eq)

-- Composition types
data Composition = Over | LeftOf | RightOf | Above | Below deriving (Show, Eq)

red, green, blue, yellow, purple :: Colour
red = RGB 255 0 0
green = RGB 0 255 0
blue = RGB 0 0 255
yellow = RGB 255 255 0
purple = RGB 128 0 128

colorToPixel :: Colour -> PixelRGB8
colorToPixel (RGB r g b) = PixelRGB8 r g b

renderDrawing :: Drawing -> IO (Image PixelRGB8)
renderDrawing drawing = do
  let width = 800
      height = 600
      backgroundColor = PixelRGB8 255 255 255  -- White
  return $ generateImage (pixelRenderer drawing backgroundColor) width height

spacing :: Int
spacing = 10

-- Rendering pixel based on Drawing
pixelRenderer :: Drawing -> PixelRGB8 -> Int -> Int -> PixelRGB8
pixelRenderer drawing bg x y =
  case drawing of
    ShapeWithColor (Circle radius) color ->
      if withinCircle (fromIntegral x) (fromIntegral y) radius 400 300
      then colorToPixel color else bg
    ShapeWithColor (Rectangle w h) color ->
      if withinRectangle (fromIntegral x) (fromIntegral y) w h 400 300
      then colorToPixel color else bg
    ShapeWithColor (Ellipse rx ry) color ->
      if withinEllipse (fromIntegral x) (fromIntegral y) rx ry 400 300
      then colorToPixel color else bg
    ShapeWithColor (Polygon vertices) color ->
      if withinPolygon (fromIntegral x) (fromIntegral y) (sortPointsClockwise vertices) 400 300
      then colorToPixel color else bg
    Transformed (Translate tx ty) innerDrawing ->
      pixelRenderer innerDrawing bg (x - round tx) (y - round ty)
    Transformed (Scale sx sy) innerDrawing ->
      let centerX = 400
          centerY = 300
          x' = round ((fromIntegral x - centerX) / sx + centerX)
          y' = round ((fromIntegral y - centerY) / sy + centerY)
      in pixelRenderer innerDrawing bg x' y'
    Transformed (Rotate angle) innerDrawing ->
      let rad = angle * pi / 180
          cosA = cos rad
          sinA = sin rad
          centerX = 400
          centerY = 300
          x' = fromIntegral x - centerX
          y' = fromIntegral y - centerY
          rotatedX = round (x' * cosA - y' * sinA + centerX)
          rotatedY = round (x' * sinA + y' * cosA + centerY)
      in pixelRenderer innerDrawing bg rotatedX rotatedY
    Transformed (Shear shX shY) innerDrawing ->
      let centerX = 400
          centerY = 300
          x' = round (fromIntegral x + shX * (fromIntegral y - centerY))
          y' = round (fromIntegral y + shY * (fromIntegral x - centerX))
      in pixelRenderer innerDrawing bg x' y'
    Composite d1 Over d2 ->
      let pixelD1 = pixelRenderer d1 bg x y
      in if pixelD1 /= bg then pixelD1 else pixelRenderer d2 bg x y
    Composite d1 Below d2 ->
      let translationY = (shapeHeight d1 `div` 2) + (shapeHeight d2 `div` 2) + spacing
      in if y >= 300 + (shapeHeight d2 `div` 2)
        then pixelRenderer d1 bg x (y - translationY)
        else pixelRenderer d2 bg x y
    Composite d1 Above d2 ->
      let translationY = (shapeHeight d1 `div` 2) + (shapeHeight d2 `div` 2) + spacing
      in if y < 300 - (shapeHeight d2 `div` 2)
        then pixelRenderer d1 bg x (y + translationY)
        else pixelRenderer d2 bg x y
    Composite d1 LeftOf d2 ->
      let translationX = (shapeWidth d1 `div` 2) + (shapeWidth d2 `div` 2) + spacing
      in if x < 400 - translationX `div` 2
        then pixelRenderer d1 bg (x + translationX) y
        else pixelRenderer d2 bg x y
    Composite d1 RightOf d2 ->
      let translationX = (shapeWidth d1 `div` 2) + (shapeWidth d2 `div` 2) + spacing
      in if x >= 400 + translationX `div` 2
        then pixelRenderer d1 bg (x - translationX) y
        else pixelRenderer d2 bg x y
    where
      withinCircle px py radius cx cy = (px - cx) ** 2 + (py - cy) ** 2 <= (radius ** 2)
      withinRectangle px py width height cx cy =
        px >= cx - width / 2 && px <= cx + width / 2 &&
        py >= cy - height / 2 && py <= cy + height / 2
      withinEllipse px py rx ry cx cy =
        ((px - cx) ** 2) / (rx ** 2) + ((py - cy) ** 2) / (ry ** 2) <= 1
      withinPolygon px py vertices cx cy =
        let translatedVertices = map (\(vx, vy) -> (vx + cx, vy + cy)) vertices
        in pointInPolygon (px, py) translatedVertices

-- To check if a point is inside a polygon or not
pointInPolygon :: (Float, Float) -> [(Float, Float)] -> Bool
pointInPolygon (px, py) vertices =
    let edges = zip vertices (tail vertices ++ [head vertices])
        crossings = length $ filter (edgeCrossesRay (px, py)) edges
    in crossings `mod` 2 == 1

-- To check if a horizontal ray from a point crosses an edge
edgeCrossesRay :: (Float, Float) -> ((Float, Float), (Float, Float)) -> Bool
edgeCrossesRay (px, py) ((x1, y1), (x2, y2)) =
    let ((leftX, leftY), (rightX, rightY)) = if y1 < y2 then ((x1, y1), (x2, y2)) else ((x2, y2), (x1, y1))
    in py > leftY && py <= rightY && px < (rightX - leftX) * (py - leftY) / (rightY - leftY) + leftX

-- Sorting in clockwise order
sortPointsClockwise :: [(Float, Float)] -> [(Float, Float)]
sortPointsClockwise points =
    let centerX = sum (map fst points) / fromIntegral (length points)
        centerY = sum (map snd points) / fromIntegral (length points)
        angle p = atan2 (snd p - centerY) (fst p - centerX)
    in sortBy (compare `on` angle) points

shapeWidth :: Drawing -> Int
shapeWidth (ShapeWithColor (Circle r) _) = round (2 * r)
shapeWidth (ShapeWithColor (Rectangle w _) _) = round w
shapeWidth (ShapeWithColor (Ellipse rx _) _) = round (2 * rx)
shapeWidth (ShapeWithColor (Polygon vertices) _) = let xs = map fst vertices in round (maximum xs - minimum xs)
shapeWidth _ = 0

shapeHeight :: Drawing -> Int
shapeHeight (ShapeWithColor (Circle r) _) = round (2 * r)
shapeHeight (ShapeWithColor (Rectangle _ h) _) = round h
shapeHeight (ShapeWithColor (Ellipse _ ry) _) = round (2 * ry)
shapeHeight (ShapeWithColor (Polygon vertices) _) = let ys = map snd vertices in round (maximum ys - minimum ys)
shapeHeight _ = 0

-- Example Drawings
-- 1. All Shapes Example
exampleAllShapes :: Drawing
exampleAllShapes =
  Composite 
      (ShapeWithColor (Circle 60) red) Over
      (Composite
          (Transformed (Translate 200 0) (ShapeWithColor (Rectangle 120 60) blue)) Over
          (Composite 
              (Transformed (Translate (-200) 0) (ShapeWithColor (Ellipse 80 40) green)) Over
              (Transformed (Translate 0 150) (ShapeWithColor (Polygon [(0, 50), (50, -50), (-50, -50)]) purple))))

-- 2. All Transformations Example
exampleAllTransformations :: Drawing
exampleAllTransformations = 
  Composite 
    (Transformed (Rotate 45) (ShapeWithColor (Ellipse 60 30) green)) Over
    (Transformed (Translate 250 150) (ShapeWithColor (Circle 40) yellow))

-- 3. All Compositions Example
exampleAllCompositions :: Drawing
exampleAllCompositions =
  Composite
    (ShapeWithColor (Ellipse 50 25) green) Above
    (Composite
      (ShapeWithColor (Polygon [(0, 50), (50, -50), (-50, -50)]) yellow) Below
      (ShapeWithColor (Rectangle 50 50) purple))