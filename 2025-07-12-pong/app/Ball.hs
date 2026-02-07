{-# LANGUAGE InstanceSigs #-}

module Ball where

import Linear
import Rendering
  ( Renderer
  , Renderable(..)
  , RenderableData
  , createVertexBufferFromVerts
  , mkCircleVerts
  )
import Graphics.GPipe
import qualified Graphics.GPipe.Context.GLFW as GLFW


-- | Game ball
data Ball = Ball
  { _ball_pos      :: V2 Float
  , _ball_size     :: Float
  , _ball_velocity :: V2 Float
  , _label         :: String
  , _ball_color    :: V4 Float
  } deriving Show

-- | Make Ball renderable in the GPipe renderer system
instance Renderable Ball where
  createRenderable :: Renderer os
                   -> Ball
                   -> ContextT GLFW.Handle os IO (RenderableData os)
  createRenderable renderer ball = do
    let pos    = _ball_pos ball
        radius = _ball_size ball / 2
        circleVerts = mkCircleVerts radius 64

    createVertexBufferFromVerts renderer circleVerts pos
