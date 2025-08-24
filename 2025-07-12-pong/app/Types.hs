{-# LANGUAGE TypeFamilies, DataKinds #-}
-- one of the key “architectural” advantages of data families over ADTs is that 
-- you don’t have to pattern match on constructors as much, because each type is 
-- a separate type rather than a variant of a single ADT.

module Types where

data Actor = Ball | Paddle

