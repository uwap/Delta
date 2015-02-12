module Main where

import Graphics.UI.GLUT
import Entity

main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  
  initialWindowSize $= Size 640 480
  initialWindowPosition $= Position 100 150
  initialDisplayMode $= [DoubleBuffered, RGBMode]
  
  _window <- createWindow "Hello World"
  
  displayCallback $= display
  reshapeCallback $= Just reshape
  
  mainLoop
 
display :: DisplayCallback
display = do
  loadIdentity
  clear [ ColorBuffer ]
  renderPrimitive Quads $ do
    render $ Player 200 200 100 100
  swapBuffers


reshape :: ReshapeCallback
reshape size@(Size width height) = do
  viewport $= (Position 0 0, size)
  matrixMode $= Projection
  loadIdentity
  ortho 0 640 480 0 (-1) 1
  matrixMode $= Modelview 0
  loadIdentity
