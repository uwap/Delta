module World where

import Graphics.UI.GLUT (renderPrimitive, PrimitiveMode(Quads))
import Control.Monad.State

type Game a = StateT World IO a

data Entity = Entity { update :: World -> Entity
                     , render :: IO ()
                     }

data World = World { entities :: [Entity] }

updateAllEntities :: Game ()
updateAllEntities = do
  w <- get
  let entityList = map (flip update w) $ entities w
  put $ World entityList

renderAllEntities :: Game ()
renderAllEntities = do
  w <- get
  liftIO . mapM_ (renderPrimitive Quads . render) $ entities w
