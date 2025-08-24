{-# LANGUAGE MultiParamTypeClasses     #-}

module Types where

import Linear.V2
import qualified Graphics.Rendering.OpenGL as GL

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
  moveActor :: a -> GameScene -> a
  createActorVertices :: a -> IO (GL.VertexArrayObject, GL.BufferObject, Int)
  isInBoundary :: a -> Boundary -> Bool

instance ActorClass Ball where
  applyCollision = undefined
  moveActor = undefined
  createActorVertices = undefined
  isInBoundary = undefined

instance ActorClass Paddle where
  applyCollision = undefined
  moveActor = undefined
  createActorVertices = undefined
  isInBoundary = undefined

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
