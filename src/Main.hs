module Main where

import Graphics.UI.GLUT
import Entity

main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  _window <- createWindow "Hello World"
  displayCallback $= display
  mainLoop
 
display :: DisplayCallback
display = do
  clear [ ColorBuffer ]
  renderPrimitive Quads $ do
    render $ Player 0 0 0.5 0.5
  flush
