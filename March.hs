{-# LANGUAGE ParallelListComp, TemplateHaskell, TypeOperators, BangPatterns, DataKinds #-}


import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL
import Graphics.Rendering.OpenGL.GLU.Matrix

import Data.Char
import Data.IORef
 
myPoints :: [(GLfloat,GLfloat,GLfloat)]
myPoints = [ (sin (2*pi*k/12), cos (2*pi*k/12), 0) | k <- [1..12] ] 

aPoints :: (GLfloat, GLfloat, GLfloat) -> GLfloat -> GLfloat -> [(GLfloat,GLfloat, GLfloat)]
aPoints start@(x, y, z) width height = [start, midPoint, midPoint, end, sideLeft, sideRight] 
  where
    end = (x + width, y, z)
    midX = x + (width / 2.0)
    midPoint = (midX,  height, z)
    sideHeight = height / 2.0
    sideStart = midX - (midX / 4.5)
    sideEnd = midX + (midX / 4.5)
    sideLeft = (sideStart, sideHeight, z)
    sideRight = (sideEnd, sideHeight, z)

bPoints :: (GLfloat, GLfloat, GLfloat) -> GLfloat -> GLfloat -> [(GLfloat,GLfloat, GLfloat)]
bPoints start@(x, y, z) width height = [start,
  (x, y + height, z),

  (x, y + height, z),
  (x + width, y + ((height * 2) / 3), z),
  (x + width, y + ((height * 2) / 3), z),
  (x, y + (height / 2), z),

  (x, y, z),
  (x + width, y + (height / 3), z),
  (x + width, y + (height / 3), z),
  (x, y + (height / 2), z)]

ePoints :: (GLfloat, GLfloat, GLfloat) -> GLfloat -> GLfloat -> [(GLfloat,GLfloat, GLfloat)]
ePoints start@(x, y, z) width height = [ start,
  (x + width, y, z),
  (x, y + height / 2, z),
  (x + ((width * 2) / 3), y + height / 2, z),
  (x, y + height, z),
  (x + width, y + height, z),
  start,
  (x, y + height, z)]

fPoints :: (GLfloat, GLfloat, GLfloat) -> GLfloat -> GLfloat -> [(GLfloat,GLfloat, GLfloat)]
fPoints start@(x, y, z) width height = [(x, y + height / 2, z),
  (x + ((width * 2) / 3), y + height / 2, z),
  (x, y + height, z),
  (x + width, y + height, z),
  start,
  (x, y + height, z)]

hPoints :: (GLfloat, GLfloat, GLfloat) -> GLfloat -> GLfloat -> [(GLfloat,GLfloat, GLfloat)]
hPoints start@(x, y, z) width height = [(x, y + height / 2, z),
  (x + width, y + height / 2, z),
  start,
  (x, y + height, z),
  (x + width, y, z),
  (x + width, y + height, z)]

iPoints :: (GLfloat, GLfloat, GLfloat) -> GLfloat -> GLfloat -> [(GLfloat,GLfloat, GLfloat)]
iPoints start@(x, y, z) width height = [start,
  (x + width, y, z),
  (x, y + height, z),
  (x + width, y + height, z),
  (x + (width / 2), y, z),
  (x + (width / 2), y + height, z)
  ]

nPoints :: (GLfloat, GLfloat, GLfloat) -> GLfloat -> GLfloat -> [(GLfloat,GLfloat, GLfloat)]
nPoints start@(x, y, z) width height = [start,
  (x, y + height, z),

  (x, y + height, z),
  (x + width, y, z),

  (x + width, y, z),
  (x + width, y + height, z)]

kPoints :: (GLfloat, GLfloat, GLfloat) -> GLfloat -> GLfloat -> [(GLfloat,GLfloat, GLfloat)]
kPoints start@(x, y, z) width height = [start,
  (x, y + height, z),
  (x, y + (height / 2), z),
  (x + width, y + height, z),
  (x, y + (height / 2), z),
  (x + width, y, z)]

lPoints :: (GLfloat, GLfloat, GLfloat) -> GLfloat -> GLfloat -> [(GLfloat,GLfloat, GLfloat)]
lPoints start@(x, y, z) width height = [start,
  (x, y + height, z),
  start,
  (x + width, y, z)]

xPoints :: (GLfloat, GLfloat, GLfloat) -> GLfloat -> GLfloat -> [(GLfloat,GLfloat, GLfloat)]
xPoints start@(x, y, z) width height = [start,
  (x + width, y + height, z),
  (x + width, y, z),
  (x, y + height, z)]

wPoints :: (GLfloat, GLfloat, GLfloat) -> GLfloat -> GLfloat -> [(GLfloat,GLfloat, GLfloat)]
wPoints start@(x, y, z) width height = [(x, y + height, z),
  (x + (width / 4), y, z),

  (x + (width / 4), y, z),
  (x + (width / 2), y + (height / 2), z),

  (x + (width / 2), y + (height / 2), z),
  (x + ((width * 3) / 4), y, z),

  (x + ((width * 3) / 4), y, z),
  (x + width, y + height, z)]

letterTransform letter 
  | letter == 'A' = aPoints
  | letter == 'B' = bPoints
  | letter == 'E' = ePoints
  | letter == 'F' = fPoints
  | letter == 'H' = hPoints
  | letter == 'I' = iPoints
  | letter == 'K' = kPoints
  | letter == 'L' = lPoints
  | letter == 'X' = xPoints
  | letter == 'W' = wPoints
  | letter == 'N' = nPoints
  | True = hPoints

writeWord :: String -> (GLfloat, GLfloat, GLfloat) -> GLfloat -> GLfloat -> [(GLfloat, GLfloat, GLfloat)]
writeWord "" _ _ _ = []
writeWord (l:ls) start@(x, y, z) width height = (transformer start width height) ++ 
  (writeWord ls (x + (width * 1.375), y, z) width height)
  where 
    transformer = letterTransform $ toUpper l

cube :: GLfloat -> IO()
cube w = do 
  renderPrimitive Quads $ do
    vertex $ Vertex3 w w w
    vertex $ Vertex3 w w (-w)
    vertex $ Vertex3 w (-w) (-w)
    vertex $ Vertex3 w (-w) w

    vertex $ Vertex3 w w w
    vertex $ Vertex3 w w (-w)
    vertex $ Vertex3 (-w) w (-w)
    vertex $ Vertex3 (-w) w w

    vertex $ Vertex3 w w w
    vertex $ Vertex3 w (-w) w
    vertex $ Vertex3 (-w) (-w) w
    vertex $ Vertex3 (-w) w w

    vertex $ Vertex3 (-w) w w
    vertex $ Vertex3 (-w) w (-w)
    vertex $ Vertex3 (-w) (-w) (-w)
    vertex $ Vertex3 (-w) (-w) w

    vertex $ Vertex3 w (-w) w
    vertex $ Vertex3 w (-w) (-w)
    vertex $ Vertex3 (-w) (-w) (-w)
    vertex $ Vertex3 (-w) (-w) w

    vertex $ Vertex3 w w (-w)
    vertex $ Vertex3 w (-w) (-w)
    vertex $ Vertex3 (-w) (-w) (-w)
    vertex $ Vertex3 (-w) w (-w)


main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  _window <- createWindow "Hello World"
  displayCallback $= display 0.1
  mainLoop
 
display :: IORef Camera -> DisplayCallback
display camera = do 
  camera <- get camera
  perspective 90 1.6 0 100
  initialDisplayMode $= [DoubleBuffered, WithSamplesPerPixel 320, WithDepthBuffer]
  multisample $= Enabled
  lineWidth $= 3
  lineSmooth $= Enabled


  clear [ColorBuffer]
  renderPrimitive Lines $
     mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) $ writeWord "BEFWIN" (-0.95, 0.0, 0.0) 0.2 0.3

  cube 0.5


  flush