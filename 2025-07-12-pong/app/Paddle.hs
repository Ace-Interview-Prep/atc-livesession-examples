{-# LANGUAGE InstanceSigs #-}

module Paddle where

import Linear
import Rendering
  ( Renderer
  , Renderable(..)
  , RenderableData
  , createVertexBufferFromVerts
  , mkRectangleVerts
  )
import Graphics.GPipe
import qualified Graphics.GPipe.Context.GLFW as GLFW

import qualified Debug.Trace as DT


-- | Game paddle
data Paddle = Paddle
  { _paddle_pos      :: V2 Float
  , _paddle_width    :: Float
  , _paddle_height   :: Float
  , _paddle_label    :: String
  } deriving Show


-- | Make Ball renderable in the GPipe renderer system
instance Renderable Paddle where
  createRenderable :: Renderer os
                   -> Paddle
                   -> ContextT GLFW.Handle os IO (RenderableData os)
  createRenderable renderer paddle = do
    let width = _paddle_width paddle
        height = _paddle_height paddle
        (V2 posX posY) = _paddle_pos paddle
        newPos = V2 (posX) (posY - (height / 2))
        verts = mkRectangleVerts width height

    createVertexBufferFromVerts renderer verts newPos
