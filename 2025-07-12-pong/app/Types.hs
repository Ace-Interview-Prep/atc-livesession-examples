{-# LANGUAGE TypeFamilies, DataKinds   #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
-- one of the key “architectural” advantages of data families over ADTs is that 
-- you don’t have to pattern match on constructors as much, because each type is 
-- a separate type rather than a variant of a single ADT.

module Types where

import Linear.V2
import qualified Graphics.Rendering.OpenGL as GL

import Helpers

-- Haskell needs a way to know which constructor an instance is at runtime.
-- If you have a value of type @Actor@, it could be either a @Ball@ or a @Paddle@.
-- Haskell internally stores a small integer or “tag” that says which constructor it is.
-- Actor memory layout:
-- +------+--------------------+
-- | tag  | fields             |
-- +------+--------------------+
-- | 0    | Ball fields        |
-- | 1    | Paddle fields      |
-- +------+--------------------+
-- the runtime checks the tag to know which branch to take.
-- This adds a tiny runtime overhead and means the memory layout must accommodate 
-- all variants, possibly with padding (alignment different var memory sizes).
--
-- With Data Families is completely separate at compile time.
-- No tag is needed — the compiler already knows the type.
-- You can call move or getPos on without pattern matching, 
-- because the type is known.

data ActorTag = BallTag | PaddleTag

data family Actor (t :: ActorTag)

-- DataKinds promotes constructors (BallTag and PaddleTag) to be used 
-- on the left side of `=` as Type. From Value Level to Type Level. 
-- the '+BallTag is just "clarity sugar". BallTag would work too.
data instance Actor 'BallTag = Ball
  { _ball_pos :: V2 Float
  , _ball_size :: Float
  , _ball_direction :: V2 Float
  }
  
data instance Actor 'PaddleTag = Paddle
  { _paddle_pos :: V2 Float
  , _paddle_width :: Float
  , _paddle_height :: Float
  }

-- Typeclass for generic operations
class ActorClass a where
  applyCollision :: a -> a
  moveActor :: a -> GameScene -> a
  hasCollided :: a -> [AnyActor] -> Bool -- [a] should be generalised
  createActorVertices :: a -> IO (GL.VertexArrayObject, GL.BufferObject, Int)
  isInBoundary :: a -> Boundary -> Bool
  pos :: a -> V2 Float
  size :: a -> Float

-- | Hasell does not usually allow instances for concrete
-- type family application. 
-- that's why to allow for the instances below you need 
-- {-# LANGUAGE FlexibleInstances         #-}
instance ActorClass (Actor 'BallTag) where
  applyCollision = undefined
  moveActor = undefined
  hasCollided _ [] = False
  hasCollided ball ((Ball pos2 size2 _):bs) =
    let
      distanceX = (getX pos) - (getX pos2)
      distanceY = (getY pos) - (getY pos2)
      distance = sqrt ((distanceX ** 2) + (distanceY ** 2))
    in
      if distance > (size/2) + (size2/2)
      then hasCollided (Ball pos size mv) bs
      else True
  createActorVertices = undefined
  isInBoundary = undefined

instance ActorClass (Actor 'PaddleTag) where
  applyCollision = undefined
  moveActor = undefined
  hasCollided _ _ = False 
  createActorVertices = undefined
  isInBoundary = undefined

-- | multiparam typeclasses pragma makes this possible - 
class Collides a b where
  collides :: a -> b -> Bool
-- TODO even using this Collide with [AnyActor] it means that with the instances of:
-- instance Collides (Actor 'BallTag) (Actor 'BallTag) where ...
-- instance Collides (Actor 'BallTag) (Actor 'PaddleTag) where ...
-- you need to know which type is what in the array [AnyActor] but it is hidden at compile time 
-- Collides is like compile-time overloading: the compiler needs to know the types statically.
-- [AnyActor] is like a box where you’ve hidden the label. You can still open the box at runtime, but the compiler can’t look inside.
-- so the issue is that using Collide a b instances, we need to know what type is the second object, but if we use an ethereogeneous list, we don't know at compile time what type it is, because we hide it behind existential type.. which if we need to use runtime pattern match.. then makes it more wirthwhile to get rid of the ethereogeneous list so that at list we keep the types simple

-- | Existential wrapper to store heterogeneous actors
-- This says: “A @AnyActor@ can wrap up any type @a@ that
-- has an ActorClass instance.”
-- requires PRAGMA # ExistentialQuantification #
data AnyActor = forall a. ActorClass a => AnyActor a

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
  { _gameScene_actors :: [AnyActor]
  , _gameScene_boundary :: Boundary
  }
