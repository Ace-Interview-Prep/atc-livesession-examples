{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications, NamedFieldPuns #-}

module Rendering
  ( Renderable (..)
  , RenderableData (..)
  , Scene (..)

  , Renderer
  , initRenderer
  , renderScene
  , destroyAll

  , createRenderableFromCircle
  , translation2D
  ) where


import Control.Monad (forM_)
import Data.Foldable (toList)
import Data.Word (Word32)
import Graphics.GPipe hiding (newVertexArray)
import Graphics.GPipe.Buffer (bufBElement)
import Graphics.GPipe.PrimitiveArray (VertexArray (..))
import qualified Graphics.GPipe.Context.GLFW as GLFW

data RenderableData os = RenderableData
  { origin :: V2 Float
  , localVerts :: [V2 Float]
  , buf :: Buffer os (B2 Float)
  , primArray :: PrimitiveArray Triangles (B2 Float)
  }

class Renderable a where
  createRenderable :: Renderer os -> a -> ContextT GLFW.Handle os IO (RenderableData os)

data Scene os = Scene
  { _renderables :: [RenderableData os]
  }

data Renderer os = Renderer
  { win :: Window os RGBAFloat ()
  , shader :: CompiledShader os ( PrimitiveArray Triangles (B2 Float)
                                , Buffer os (Uniform (B2 Float)) )
  }

initRenderer
  :: V2 Int
  -> ContextT GLFW.Handle os IO (Renderer os)
initRenderer (V2 w h) = do
  let wc = (GLFW.defaultWindowConfig "GPipe Ping Pong")
        { GLFW.configWidth = w
        , GLFW.configHeight = h
        }
  win <- newWindow wc RGB8

  shader <- compileShader $ do
    prims <- toPrimitiveStream fst
    utrans <- getUniform snd

    let prims4 = fmap (\(V2 x y) -> V4 (x + utrans^._x) (y + utrans^._y) 0 1) prims

    -- Rasterize with default 2D settings (no depth, no culling)
    frags <- rasterize (const (FrontAndBack, PolygonFill, NoOffset, (V2 0 0, V2 1 1))) prims4

    -- Set fragment color
    let colors = pure (V4 1 1 1 1) <$ frags
    drawWindowColor (const (win, ContextColorOption NoBlending (V4 True True True True))) colors

  pure Renderer{ win, shader }

destroyAll :: Renderer os -> ContextT GLFW.Handle os IO ()
destroyAll Renderer{win} = deleteWindow win

renderScene :: Renderer os-> Scene os -> ContextT GLFW.Handle os IO ()
renderScene Renderer{win, shader} (Scene rs) = do
  render $ clearWindowColor win 0
  forM_ rs $ \RenderableData{ primArray, origin } -> do
    u <- newBuffer 1
    writeBuffer u 0 [origin]
    render $ shader (primArray, u)
  swapWindowBuffers win


type GLFWContext os a = ContextT GLFW.Handle os IO a


createVertexBufferFromCircle :: Renderer os -> [V2 Float] -> V2 Float -> GLFWContext os (RenderableData os)
createVertexBufferFromCircle _ localVerts origin = do
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
      let pairs = zip rim (drop 1 rim <> take 1 rim) -- wrap last to first
      in concatMap (\(a,b) -> [c, a, b]) pairs

-- Helper to generate a 2D circle (origin-centered) as a triangle fan.
mkCircleVerts :: Float -> Int -> [V2 Float]
mkCircleVerts radius steps =
  let center = V2 0 0
      rim    = [ V2 (radius * sin t) (radius * cos t)
               | i <- [0 .. steps-1]
               , let t = fromIntegral i * (2*pi / fromIntegral steps)
               ]
  in triangulateFan (center : rim)




















-- import SDL
-- import Foreign.Ptr
-- import Control.Monad
-- import qualified Linear
-- import qualified SDL.Vect as S
-- import qualified Graphics.Rendering.OpenGL as GL
-- import qualified Data.Vector.Storable as V
-- import Control.Exception (SomeException, try, throwIO)

-- data RenderableData = RenderableData
--   { origin :: V2 Float
--   , vao :: GL.VertexArrayObject
--   , vbo :: GL.BufferObject
--   , mode :: GL.PrimitiveMode
--   , vertices :: [V2 Float]
--   }

-- class Renderable a where
--   createRenderable :: a -> IO RenderableData

-- data Scene = Scene
--   { _renderables :: [RenderableData]
--   }

-- renderScene :: Window -> GL.Program -> GL.UniformLocation -> Scene -> IO ()
-- renderScene window program loc scene = do
--   clearScene
--   setShaderProgram program
--   forM (_renderables scene) (draw loc)
--   glSwapWindow window

-- clearScene :: IO ()
-- clearScene = do
--   GL.clearColor $= GL.Color4 0 0 0 1
--   GL.clear [GL.ColorBuffer]

-- draw :: GL.UniformLocation -> RenderableData -> IO ()
-- draw loc (RenderableData origin vao _ pmode verts) = do
--   GL.bindVertexArrayObject $= Just vao
--   matrix <- translationMatrix origin
--   GL.uniform loc $= matrix
--   GL.drawArrays pmode 0 (fromIntegral $ length verts)


-- translationMatrix :: V2 Float -> IO (GL.GLmatrix GL.GLfloat)
-- translationMatrix (V2 x y)
--   = GL.newMatrix GL.ColumnMajor
--     [ 1.0, 0.0, 0.0, 0.0
--     , 0.0, 1.0, 0.0, 0.0
--     , 0.0, 0.0, 1.0, 0.0
--     , x, y, 0.0, 1.0
--     ]


-- flattenVertices :: (Fractional a, V.Storable a) => [V2 a] -> V.Vector a
-- flattenVertices = V.fromList . unwrapV2

-- unwrapV2 :: Fractional a => [V2 a] -> [a]
-- unwrapV2 [] = []
-- unwrapV2 ((V2 x y):xs) = x : y : 0.0 : unwrapV2 xs

-- setViewport :: GL.Position -> GL.Size -> IO ()
-- setViewport pos size = do
--   GL.viewport $= (pos, size)

-- setShaderProgram :: GL.Program -> IO ()
-- setShaderProgram program = do
--   GL.currentProgram $= Just program

-- createGLContext :: Window -> IO GLContext
-- createGLContext window = do
--   glContextResult <- try $ glCreateContext window
--   glContext <- case glContextResult of
--     Left (err :: SomeException) -> throwIO $ userError $ "OpenGL context creation failed: " ++ show err
--     Right ctx -> return ctx
--   return glContext

-- createRenderableData :: V2 Float -> [V2 Float] -> GL.PrimitiveMode -> IO RenderableData
-- createRenderableData origin verts pmode = do
--   (vao, vbo) <- createVertexObject verts
--   return $ RenderableData origin vao vbo pmode verts

-- createVertexObject :: [V2 Float] -> IO (GL.VertexArrayObject, GL.BufferObject)
-- createVertexObject verts = do
--   let verts' = flattenVertices verts

--   vao <- GL.genObjectName
--   GL.bindVertexArrayObject $= Just vao

--   vbo <- GL.genObjectName
--   GL.bindBuffer GL.ArrayBuffer $= Just vbo
--   V.unsafeWith verts' $ \ptr ->
--     GL.bufferData GL.ArrayBuffer $= (fromIntegral (V.length verts' * 4), ptr, GL.StaticDraw)

--   GL.vertexAttribArray (GL.AttribLocation 0) $= GL.Enabled
--   GL.vertexAttribPointer (GL.AttribLocation 0) $=
--     (GL.ToFloat, GL.VertexArrayDescriptor 3 GL.Float 0 nullPtr)

--   GL.bindBuffer GL.ArrayBuffer $= Nothing
--   GL.bindVertexArrayObject $= Nothing

--   return (vao, vbo)

-- destroyAll :: Window -> GLContext -> [GL.Program] -> [GL.VertexArrayObject] -> [GL.BufferObject] -> IO ()
-- destroyAll window context programs vaos vbos = do
--   GL.deleteObjectNames programs
--   GL.deleteObjectNames vaos
--   GL.deleteObjectNames vbos
--   glDeleteContext context
--   destroyWindow window
