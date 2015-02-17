module Entity where

import World
import Graphics.UI.GLUT hiding (get) -- (Size, Position, vertex, Vertex3, GLfloat)
import Control.Monad.State (liftIO, get)

player :: Position -> Size -> Entity
player pos@(Position x y) size = Entity (u x y size) (r pos size)
      where
        u x y s = do
          w <- get
          let r = map (k . fst) $ keyChanges w
          return $ flip player s $ Position (x + sum r) y
        r p s = quad p s
        k key = do
          case key of
            Char 'd' -> 1 :: GLint
            _        -> 0 :: GLint

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
