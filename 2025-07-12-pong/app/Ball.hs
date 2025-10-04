{-# LANGUAGE InstanceSigs #-}

module Ball where

import Linear
import Rendering (Renderable (..), RenderableData, createRenderableData)
import qualified Graphics.Rendering.OpenGL as GL

data Ball = Ball
    { _ball_pos :: V2 Float
    , _ball_size :: Float
    , _ball_velocity :: V2 Float
    , _label :: String
    } deriving Show

instance Renderable Ball where
  createRenderable :: Ball -> IO RenderableData
  createRenderable ball = do
    let verts = createBallVertices ball
    createRenderableData (_ball_pos ball) verts GL.TriangleFan

createBallVertices :: Ball -> [V2 Float]
createBallVertices (Ball ballPos size _ _) =
  let V2 ballX ballY = ballPos
      ballRadius = size / 2
  in
    createCircleVertices ballPos ballRadius

createCircleVertices :: V2 Float -> Float -> [V2 Float]
createCircleVertices pos radius = [pos] <> ((calculateCircumferencePoint pos radius) <$> [1..360])
  where
    calculateCircumferencePoint :: V2 Float -> Float -> Int -> V2 Float
    calculateCircumferencePoint pos r th = V2 (r * (sin (fromIntegral th))) (r * (cos (fromIntegral th))) + pos
