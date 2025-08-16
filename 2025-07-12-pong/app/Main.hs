{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import SDL
import SDL.Video.OpenGL (glCreateContext)
import Prelude hiding (log)
import Foreign.Ptr
import Foreign.C.Types (CInt)
import Control.Exception (SomeException, try, throwIO)
import Control.Monad (unless, void)
import Data.ByteString (ByteString)
import GHC.Int
import qualified Graphics.Rendering.OpenGL as GL
import qualified Data.Vector.Storable as V
import Graphics.Rendering.OpenGL.GL.CoordTrans
import Shaders (vertexShaderSource, fragmentShaderSource)

import Data.Time.Clock.POSIX (getPOSIXTime)

getUnixTimeMillis :: IO Integer
getUnixTimeMillis = do
  time <- getPOSIXTime
  return $ floor (time * 1000)

data Ball = Ball
  { _ball_pos :: V2 Float
  , _ball_size :: Float
  }

newtype Mass = Mass Float
newtype Gravity = Gravity Float

gravity :: Gravity
gravity = Gravity 9.80665

-- applyGravity :: V2 -> Mass -> V2
-- applyGravity pos mass = pos - (mass * gravity)







screenWidth :: (Num a) => a
screenWidth = 800

screenHeight :: (Num a) => a
screenHeight = 800

main :: IO ()
main = do
  initializeAll

  let windowConfig = defaultWindow
        { windowInitialSize = V2 screenWidth screenHeight
        , windowGraphicsContext = OpenGLContext defaultOpenGL
                                  { glProfile = Core Normal 3 3
                                  }
        }

  putStrLn $ show (windowGraphicsContext windowConfig)

  windowResult <- try $ createWindow "Ping Pong" windowConfig

  window <- case windowResult of
    Left (err :: SomeException) -> throwIO $ userError $ "Window creation failed: " ++ show err
    Right w -> do
      putStrLn "Window created successfully"
      return w

  glContextResult <- try $ glCreateContext window
  glContext <- case glContextResult of
    Left (err :: SomeException) -> throwIO $ userError $ "OpenGL context creation failed: " ++ show err
    Right ctx -> return ctx

  GL.viewport $= (Position 0 0, Size screenWidth screenHeight)

  programResult <- try $ createShaderProgramFromSource vertexShaderSource fragmentShaderSource
  program <- case programResult of
    Left (err :: SomeException) -> throwIO $ userError $ "Shader program creation failed: " ++ show err
    Right prog -> return prog

  GL.currentProgram $= Just program

  (triangleVao, triangleVbo) <- createTriangleVertices

  let ball = Ball (V2 0 0) 0.1
  currentUnixTime <- getUnixTimeMillis
  gameLoop window triangleVao ball currentUnixTime program

  GL.deleteObjectNames [program]
  GL.deleteObjectNames [triangleVao]--, ballVao]
  GL.deleteObjectNames [triangleVbo]--, ballVbo]
  glDeleteContext glContext
  destroyWindow window
  quit


isQuitEvent :: EventPayload -> Bool
isQuitEvent e = case e of
  QuitEvent -> True
  KeyboardEvent keyboardEvent ->
    keyboardEventKeyMotion keyboardEvent == Pressed &&
    keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeEscape
  _ -> False



createBallVertices :: Ball -> IO (GL.VertexArrayObject, GL.BufferObject, Int)
createBallVertices ball = do
  let V2 ballX ballY = _ball_pos ball
      ballRadius = _ball_size ball / 2
      vertices
        = V.fromList
        $ concat [ [ballX, ballY, 0.0]
                 ] ++ unwrapV2 (createCircleVertices ballRadius)
      numVertices = (V.length vertices) `div` 3

  vao <- GL.genObjectName
  GL.bindVertexArrayObject $= Just vao

  vbo <- GL.genObjectName
  GL.bindBuffer GL.ArrayBuffer $= Just vbo
  V.unsafeWith vertices $ \ptr ->
    GL.bufferData GL.ArrayBuffer $= (fromIntegral (V.length vertices * 4), ptr, GL.StaticDraw)

  GL.vertexAttribArray (GL.AttribLocation 0) $= GL.Enabled
  GL.vertexAttribPointer (GL.AttribLocation 0) $=
    (GL.ToFloat, GL.VertexArrayDescriptor 3 GL.Float 0 nullPtr)

  GL.bindBuffer GL.ArrayBuffer $= Nothing
  GL.bindVertexArrayObject $= Nothing

  return (vao, vbo, numVertices)

  where
    unwrapV2 :: [V2 Float] -> [Float]
    unwrapV2 [] = []
    unwrapV2 ((V2 x y):xs) = x : y : 0.0 : unwrapV2 xs



createCircleVertices :: Float -> [V2 Float]
createCircleVertices radius = (calculateCircumferencePoint radius) <$> [1..360]


calculateCircumferencePoint :: Float -> Int -> V2 Float
calculateCircumferencePoint r th = V2 (r * (sin (fromIntegral th))) (r * (cos (fromIntegral th)))


createTriangleVertices :: IO (GL.VertexArrayObject, GL.BufferObject)
createTriangleVertices = do
  let vertices = V.fromList
        [ 0.0, 0.5, 0.0
        , (-0.5), (-0.5), 0.0
        , 0.5, -0.5, 0.0
        ] :: V.Vector Float

  vao <- GL.genObjectName
  GL.bindVertexArrayObject $= Just vao

  vbo <- GL.genObjectName
  GL.bindBuffer GL.ArrayBuffer $= Just vbo
  V.unsafeWith vertices $ \ptr ->
    GL.bufferData GL.ArrayBuffer $= (fromIntegral (V.length vertices * 4), ptr, GL.StaticDraw)

  GL.vertexAttribArray (GL.AttribLocation 0) $= GL.Enabled
  GL.vertexAttribPointer (GL.AttribLocation 0) $=
    (GL.ToFloat, GL.VertexArrayDescriptor 3 GL.Float 0 nullPtr)

  GL.bindBuffer GL.ArrayBuffer $= Nothing
  GL.bindVertexArrayObject $= Nothing

  return (vao, vbo)


createShaderProgramFromSource :: ByteString -> ByteString -> IO GL.Program
createShaderProgramFromSource vsrc fsrc = do
  vertexShader <- compileShaderFromSource GL.VertexShader vsrc
  fragmentShader <- compileShaderFromSource GL.FragmentShader fsrc
  program <- GL.createProgram
  GL.attachShader program vertexShader
  GL.attachShader program fragmentShader
  GL.linkProgram program
  return program



compileShaderFromSource :: GL.ShaderType -> ByteString -> IO GL.Shader
compileShaderFromSource shaderType source = do
  shader <- GL.createShader shaderType
  GL.shaderSourceBS shader $= source
  _ <- GL.compileShader shader
  ok <- GL.get (GL.compileStatus shader)
  unless ok $ do
    log <- GL.get (GL.shaderInfoLog shader)
    putStrLn $ "Shader compile log: " ++ log
  return shader


gameLoop :: Window -> GL.VertexArrayObject -> Ball -> Integer -> GL.Program -> IO ()
gameLoop window triangleVao ball prevTime program = do
  events <- pollEvents
  currentUnixTime <- getUnixTimeMillis

  let quit = any (isQuitEvent . eventPayload) events
      timeDelta = currentUnixTime - prevTime

  -- Clear the screen
  GL.clearColor $= GL.Color4 0 0 0 1
  GL.clear [GL.ColorBuffer]

  let movementVector = V2 0.0 (-0.01)
      newBall = Ball ((_ball_pos ball) + movementVector) (_ball_size ball)

  (ballVao, ballVbo, ballNumVertices) <- createBallVertices newBall

  -- OUR ENTIRE GAME

  -- WE NEED TIME DELTA HERE


  -- Draw triangle  
  --GL.bindVertexArrayObject $= Just triangleVao
  --GL.drawArrays GL.Triangles 0 3

  GL.currentProgram $= Just program
  GL.bindVertexArrayObject $= Just ballVao
  GL.drawArrays GL.TriangleFan 0 (fromIntegral ballNumVertices)
  -- Swap buffers
  glSwapWindow window

  unless quit $ gameLoop window triangleVao newBall currentUnixTime program
