module Main where

import World
import Entity

import Graphics.UI.GLUT
import Data.IORef
import Control.Monad.State

data GameState = GameState {
                 world :: IORef World
               , timerMillis :: Timeout
               }

initializeGameState :: IO GameState
initializeGameState = do
  w <- newIORef $ World [] []
  return $ GameState w 20

main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize

  initialWindowSize $= Size 640 480
  initialWindowPosition $= Position 100 150
  initialDisplayMode $= [DoubleBuffered, RGBMode]

  state <- initializeGameState
  world state $= World [player (Position 200 200) (Size 100 100)] []

  _window <- createWindow "Hello World"

  displayCallback $= display state
  reshapeCallback $= Just reshape
  keyboardMouseCallback $= Just (keyboardMouseHandler state)

  addTimerCallback (timerMillis state) (timer state)

  mainLoop

display :: GameState -> DisplayCallback
display state = do
  loadIdentity
  clear [ ColorBuffer ]
  w <- readIORef (world state)
  result <- runStateT updateGame w
  world state $= snd result
  swapBuffers

updateGame :: Game ()
updateGame = updateAllEntities >> renderAllEntities >> clearKeyboardStates

reshape :: ReshapeCallback
reshape size = do
  viewport $= (Position 0 0, size)
  matrixMode $= Projection
  loadIdentity
  ortho 0 640 480 0 (-1) 1
  matrixMode $= Modelview 0
  loadIdentity

timer :: GameState -> TimerCallback
timer state = do
  postRedisplay Nothing
  addTimerCallback (timerMillis state) (timer state)

keyboardMouseHandler :: GameState -> KeyboardMouseCallback
keyboardMouseHandler state key keyState _ _ = do
  w <- readIORef $ world state
  world state $= World (entities w) ((key, keyState) : keyChanges w)
