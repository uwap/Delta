module World where

import Graphics.UI.GLUT (renderPrimitive, PrimitiveMode(Quads))
import Control.Monad.State

type Game a = StateT World IO a

data Entity = Entity { update :: Game Entity
                     , render :: Game ()
                     }

data World = World { entities :: [Entity] }

updateAllEntities :: Game ()
updateAllEntities = do
  entityList <- get >>= sequence . map update . entities
  put $ World entityList

renderAllEntities :: Game ()
renderAllEntities = do
    w <- get
    mapM_ render $ entities w
