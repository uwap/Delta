module World where

import Graphics.UI.GLUT

data Entity = Entity { update :: World -> Entity
                     , render :: IO ()
                     }

data World = World { entities :: [Entity] }

updateAllEntities :: World -> World
updateAllEntities w = World (map (flip update w) $ entities w)

renderAllEntities :: World -> IO ()
renderAllEntities w = mapM_ (renderPrimitive Quads . render) $ entities w
