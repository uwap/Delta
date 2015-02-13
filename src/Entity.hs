{-# LANGUAGE ExistentialQuantification #-}
module Entity where

import Graphics.UI.GLUT

class Entity a where
  render :: a -> IO ()
  update :: a -> a

-- Sadly we need to use this wrapper to be able to
-- create types like [EntityType]
data EntityType = forall a. Entity a => EntityType a

data Player = Player { x :: GLfloat, y :: GLfloat, width :: GLfloat, height :: GLfloat }

instance Entity Player where
  render (Player x y w h) = quad x y w h >> print x
  update (Player x y w h) = Player (x + 1) y w h

quad :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()
quad x y w h = do
  vertex3f x y 0
  vertex3f x (y + h) 0
  vertex3f (x + w) (y + h) 0
  vertex3f (x + w) y 0

vertex3f :: GLfloat -> GLfloat -> GLfloat -> IO ()
vertex3f x y z = vertex $ Vertex3 x y z
