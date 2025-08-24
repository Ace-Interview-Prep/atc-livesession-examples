module Helpers where

import Linear.V2

getX :: V2 a -> a
getX (V2 x _) = x

getY :: V2 a -> a
getY (V2 _ y) = y
