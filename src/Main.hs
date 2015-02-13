module Main where

import Graphics.UI.GLUT
import Entity
import Data.IORef

data GameState = GameState {
                 entities :: IORef [EntityType]
               , timerMillis :: Timeout
               }

initializeGameState :: IO GameState
initializeGameState = do
  entities <- newIORef []
  return $ GameState entities 20

main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize

  initialWindowSize $= Size 640 480
  initialWindowPosition $= Position 100 150
  initialDisplayMode $= [DoubleBuffered, RGBMode]

  state <- initializeGameState
  entities state $= [EntityType $ Player 200 200 100 100]

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
  entityList <- readIORef $ entities state
  updatedEntities <- mapM renderEntity entityList
  entities state $= updatedEntities
  swapBuffers

renderEntity :: EntityType -> IO EntityType
renderEntity (EntityType e) = do
  renderPrimitive Quads $ render e
  return . EntityType $ update e

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
keyboardMouseHandler state key keyState _ _ = return ()
