{-# LANGUAGE MultiParamTypeClasses     #-}

module Types where

import Linear.V2
import qualified Graphics.Rendering.OpenGL as GL
import qualified Data.Vector.Storable as V

import Helpers

-- Back to ADTs after realisign that:
-- existentials give heterogeneity but force runtime checks; 
-- separate typed collections preserve compile-time type safety and performance.
-- check commit 9bb0dc1 for more notes. (or branch data-families-existential-types)

data Ball = Ball
  { _ball_pos :: V2 Float
  , _ball_size :: Float
  , _ball_direction :: V2 Float
  }
  
data Paddle = Paddle
  { _paddle_pos :: V2 Float
  , _paddle_width :: Float
  , _paddle_height :: Float
  }

-- Typeclass for generic operations
class ActorClass a where
  applyCollision :: a -> a
  moveActor :: a -> a
  isInBoundary :: a -> Boundary -> Bool
  createActorVertices :: a -> IO (GL.VertexArrayObject, GL.BufferObject, Int)

instance ActorClass Ball where
  applyCollision (Ball pos size mv) = Ball pos size (-mv)
  moveActor (Ball pos size mv) = Ball (pos + mv) size mv
  isInBoundary (Ball pos size _) boundary
    =  (getX pos - size < _boundary_left boundary)
    || (getX pos + size > _boundary_right boundary)
    || (getY pos - size < _boundary_top boundary)
    || (getY pos + size > _boundary_bottom boundary)
  createActorVertices (Ball ballPos size _) = do
    let V2 ballX ballY = ballPos
        ballRadius = size / 2
        vertices
          = V.fromList
          $ concat [ [ballX, ballY, 0.0]
                   ] ++ unwrapV2 (createCircleVertices ballPos ballRadius)
        numVertices = (V.length vertices) `div` 3

    vao <- GL.genObjectName
    GL.bindVertexArrayObject GL.$= Just vao

    vbo <- GL.genObjectName
    GL.bindBuffer GL.ArrayBuffer GL.$= Just vbo
    V.unsafeWith vertices $ \ptr ->
      GL.bufferData GL.ArrayBuffer GL.$= (fromIntegral (V.length vertices * 4), ptr, GL.StaticDraw)

    GL.vertexAttribArray (GL.AttribLocation 0) GL.$= GL.Enabled
    GL.vertexAttribPointer (GL.AttribLocation 0) GL.$=
      (GL.ToFloat, GL.VertexArrayDescriptor 3 GL.Float 0 nullPtr)

    GL.bindBuffer GL.ArrayBuffer GL.$= Nothing
    GL.bindVertexArrayObject GL.$= Nothing

    return (vao, vbo, numVertices)

    where
      unwrapV2 :: [V2 Float] -> [Float]
      unwrapV2 [] = []
      unwrapV2 ((V2 x y):xs) = x : y : 0.0 : unwrapV2 xs

instance ActorClass Paddle where
  applyCollision (Paddle pos width height) = Paddle pos width height
  moveActor (Paddle pos width height) = Paddle pos width height
  isInBoundary (Paddle pos width height) boundary
    =  (getX pos - width < _boundary_left boundary)
    || (getX pos + width > _boundary_right boundary)
    || (getY pos - height < _boundary_top boundary)
    || (getY pos + height > _boundary_bottom boundary)
  createActorVertices = undefined

-- | multiparam typeclasses pragma makes this possible - 
class Collides a b where
  collides :: a -> b -> Bool

instance Collides Ball Ball where
  collides (Ball pos size _) (Ball pos2 size2 _) =
    let
      distanceX = getX pos - getX pos2
      distanceY = getY pos - getY pos2
      distance = sqrt ((distanceX ** 2) + (distanceY ** 2))
    in
      (distance <= ((size / 2) + (size2 / 2))) || False 

-- | TODO need to revisit this calculation, I don't think is correct.
--   not sure if position is at the center for both or not.
instance Collides Ball Paddle where
  collides (Ball posB r _) (Paddle posP w h) =
    let (xBall, yBall)     = (getX posB, getY posB)
        (xPaddle, yPaddle) = (getX posP, getY posP)
    in xBall+r >= xPaddle   && 
       xBall-r <= xPaddle+w && 
       yBall+r >= yPaddle   && 
       yBall-r <= yPaddle+h

instance Collides Paddle Ball where
  collides = flip collides

data Actors = Actors
  { balls   :: [Ball]
  , paddles :: [Paddle]
  }

--------------------------------------------
-- Transfered from Main: original Kept Types
--
data Boundary = Boundary
  { _boundary_left :: Float
  , _boundary_right :: Float
  , _boundary_top :: Float
  , _boundary_bottom :: Float
  }

data GameScene = GameScene
  { _gameScene_actors :: Actors
  , _gameScene_boundary :: Boundary
  }
