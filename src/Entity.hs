module Entity where

import World
import Graphics.UI.GLUT hiding (get) -- (Size, Position, vertex, Vertex3, GLfloat)
import Control.Monad.State (liftIO, get)
import Linear.V2

player :: V2 GLfloat -> V2 GLfloat -> Entity
player pos@(V2 x y) size = Entity (u x y size) (r pos size)
      where
        u x y s = do
          w <- get
          let r = map (k . fst) $ keyChanges w
          return $ flip player s $ V2 (x + sum r) y
        r p s = quad p s
        k key = do
          case key of
            Char 'd' -> 1
            Char 'a' -> -1
            _        -> 0

quad :: (Num a, VertexComponent a) => V2 a -> V2 a -> Game ()
quad (V2 x y) (V2 w h) = liftIO $ renderPrimitive Quads $ do
  vertex2 x y
  vertex2 x (y + h)
  vertex2 (x + w) (y + h)
  vertex2 (x + w) y

vertex2 :: VertexComponent a => a -> a -> IO ()
vertex2 x y = vertex $ Vertex2 x y
