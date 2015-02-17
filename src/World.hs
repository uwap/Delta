module World where

import Graphics.UI.GLUT (renderPrimitive, PrimitiveMode(Quads), Key, KeyState)
import Control.Monad.State

type Game a = StateT World IO a

data Entity = Entity { update :: Game Entity
                     , render :: Game ()
                     }

data World = World { entities :: [Entity]
                   , keyChanges :: [(Key, KeyState)]
                   }

updateAllEntities :: Game ()
updateAllEntities = do
  w <- get
  entityList <- sequence . map update . entities $ w
  put $ World entityList (keyChanges w)

renderAllEntities :: Game ()
renderAllEntities = do
  w <- get
  mapM_ render $ entities w

clearKeyboardStates :: Game ()
clearKeyboardStates = do
  w <- get
  put $ World (entities w) []
