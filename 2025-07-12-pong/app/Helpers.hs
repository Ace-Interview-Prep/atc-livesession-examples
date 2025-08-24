module Helpers where

import Linear.V2

getX :: V2 a -> a
getX (V2 x _) = x

getY :: V2 a -> a
getY (V2 _ y) = y

createCircleVertices :: V2 Float -> Float -> [V2 Float]
createCircleVertices pos radius = (calculateCircumferencePoint pos radius) <$> [1..360]

calculateCircumferencePoint :: V2 Float -> Float -> Int -> V2 Float
calculateCircumferencePoint pos r th = V2 (r * (sin (fromIntegral th))) (r * (cos (fromIntegral th))) + pos
