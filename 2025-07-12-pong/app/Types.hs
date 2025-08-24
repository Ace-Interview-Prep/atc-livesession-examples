{-# LANGUAGE TypeFamilies, DataKinds #-}
-- one of the key “architectural” advantages of data families over ADTs is that 
-- you don’t have to pattern match on constructors as much, because each type is 
-- a separate type rather than a variant of a single ADT.

module Types where

import Linear.V2

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

