{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications, NamedFieldPuns #-}

module Rendering
  ( Renderable (..)
  , RenderableData (..)
  , Scene (..)
  , Renderer (..)
  , GLFWContext
  , initRenderer
  , renderScene
  , destroyAll
  , triangulateFan
  , mkCircleVerts
  , mkRectangleVerts
  , createVertexBufferFromVerts
  , translation2D
  ) where


import Control.Monad (forM_)
import Graphics.GPipe hiding (newVertexArray)
import Graphics.GPipe.PrimitiveArray()
import qualified Graphics.GPipe.Context.GLFW as GLFW
import Control.Lens.Getter ((^.))

type GLFWContext os a = ContextT GLFW.Handle os IO a

class Renderable a where
  createRenderable :: Renderer os -> a -> GLFWContext os (RenderableData os)

data Scene os = Scene
  { _renderables :: [RenderableData os]
  }


data RenderableData os = RenderableData
  { origin :: V2 Float
  , localVerts :: [V2 Float]
  , buf :: Buffer os (B2 Float)
  , primArray :: PrimitiveArray Triangles (B2 Float)
  }

data Renderer os = Renderer
  { win :: Window os RGBAFloat ()
  , shader :: CompiledShader os ( PrimitiveArray Triangles (B2 Float)
                                , (Buffer os (Uniform (B2 Float)), Int)
                                )
  }


initRenderer
  :: V2 Int
  -> ContextT GLFW.Handle os IO (Renderer os)
initRenderer (V2 w h) = do
  let wc = (GLFW.defaultWindowConfig "GPipe Ping Pong")
        { GLFW.configWidth = w
        , GLFW.configHeight = h
        }

  win <- newWindow (WindowFormatColor RGBA8) wc

  shader <- compileShader $ do
    prims <- toPrimitiveStream fst
    utrans <- getUniform snd

    let prims4 = fmap (\(V2 x y) ->
                         ( V4 (x + utrans^._x) (y + utrans^._y) 0 1
                         , V4 1 1 1 1 :: V4 VFloat  -- fragment color (RGBA)
                         )
                      ) prims

    -- Rasterize with default 2D settings (no depth, no culling)
    let viewport = ViewPort (V2 0 0) (V2 w h)
    frags <- rasterize (const (Front, viewport, DepthRange 0 1)) prims4

    -- Set fragment color
    drawWindowColor (const (win, ContextColorOption NoBlending (V4 True True True True))) frags

  pure Renderer{ win, shader }

destroyAll :: Renderer os -> ContextT GLFW.Handle os IO ()
destroyAll Renderer{win} = deleteWindow win

renderScene :: Renderer os-> Scene os -> ContextT GLFW.Handle os IO ()
renderScene Renderer{win, shader} (Scene rs) = do
  render $ clearWindowColor win 0
  forM_ rs $ \RenderableData{ primArray, origin } -> do
    (u :: Buffer os (Uniform (B2 Float))) <- newBuffer 1
    writeBuffer u 0 [origin]
    render $ shader (primArray, (u, 0))
  swapWindowBuffers win


createVertexBufferFromVerts :: Renderer os -> [V2 Float] -> V2 Float -> GLFWContext os (RenderableData os)
createVertexBufferFromVerts _ localVerts origin = do
  buf <- newBuffer (length localVerts)
  writeBuffer buf 0 localVerts

  let vertexArray = newVertexArray buf
      primArray = toPrimitiveArray TriangleList vertexArray

  pure RenderableData
    { origin
    , localVerts
    , buf
    , primArray
    }


newVertexArray :: Buffer os a -> VertexArray t a
newVertexArray buffer = VertexArray (bufferLength buffer) 0 $ bufBElement buffer


translation2D :: RenderableData os -> V2 Float -> RenderableData os
translation2D rd newOrigin = rd { origin = newOrigin }


-- Triangulate a triangle fan (center + rim) into a TriangleList GPipe likes.
-- Input must be model-space (center at 0,0). We construct (c, p_i, p_{i+1}) triplets.
triangulateFan :: [V2 Float] -> [V2 Float]
triangulateFan verts =
  case verts of
    []     -> []
    [_]    -> []
    (c:rim) ->
      let pairs = zip rim (drop 1 rim <> take 1 rim)
      in concatMap (\(a,b) -> [c, a, b]) pairs


-- Helper to generate a 2D circle (origin-centered) as a triangle fan.
mkCircleVerts :: Float -> Int -> [V2 Float]
mkCircleVerts radius steps =
  let center = V2 0 0
      rim    = [ V2 (radius * sin t) (radius * cos t)
               | i <- [0 .. steps-1]
               , let t = fromIntegral i * (2*pi / fromIntegral steps)
               ]
  in triangulateFan (reverse (center : rim))


mkRectangleVerts :: Float -> Float -> [V2 Float]
mkRectangleVerts width height =
  [ V2 0.0 0.0, V2 width 0.0, V2 width height
  , V2 width height, V2 0.0 height, V2 0.0 0.0
  ]
