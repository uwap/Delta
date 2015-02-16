module Entity where

import World
import Graphics.UI.GLUT

player :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> Entity
player x y w h = Entity (u x y w h) (r x y w h)
      where
        u x = player (x + 1)
        r = quad

quad :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()
quad x y w h = do
  vertex3f x y 0
  vertex3f x (y + h) 0
  vertex3f (x + w) (y + h) 0
  vertex3f (x + w) y 0

vertex3f :: GLfloat -> GLfloat -> GLfloat -> IO ()
vertex3f x y z = vertex $ Vertex3 x y z
