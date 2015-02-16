module Entity where

import World
import Graphics.UI.GLUT -- (Size, Position, vertex, Vertex3, GLfloat)
import Control.Monad.State (liftIO)

player :: Position -> Size -> Entity
player pos@(Position x y) size@(Size w h) = Entity (u x y size) (r pos size)
      where
        u x y s = return $ player (Position (x + 1) y) s
        r p s = quad p s

quad :: Position -> Size -> Game ()
quad (Position x y) (Size w h) = liftIO $ renderPrimitive Quads $ do
  vertex2i x y
  vertex2i x (y + h)
  vertex2i (x + w) (y + h)
  vertex2i (x + w) y

vertex3f :: GLfloat -> GLfloat -> GLfloat -> IO ()
vertex3f x y z = vertex $ Vertex3 x y z

vertex2i :: GLint -> GLint -> IO ()
vertex2i x y = vertex $ Vertex2 x y
