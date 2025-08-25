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

import Types
import Helpers

getUnixTimeMillis :: IO Integer
getUnixTimeMillis = do
  time <- getPOSIXTime
  return $ floor (time * 1000)

newtype Mass = Mass Float
newtype Gravity = Gravity Float

gravity :: Gravity
gravity = Gravity 9.80665

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

  print (windowGraphicsContext windowConfig)

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

  let playerBall = Ball (V2 0 0) 0.1 (V2 0.0 (-0.0005))
      otherBall = Ball (V2 0 (-0.5)) 0.1 (V2 0.0 0.0)
      dummyPaddle = Paddle (V2 1 0) 0.1 0.3
      boundary = Boundary 0.0 0.0 0.0 0.0
      gameScene = GameScene (Actors [playerBall, otherBall] [dummyPaddle]) boundary

  currentUnixTime <- getUnixTimeMillis
  gameLoop window triangleVao gameScene currentUnixTime program

  GL.deleteObjectNames [program]
  GL.deleteObjectNames [triangleVao]
  GL.deleteObjectNames [triangleVbo]
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

createTriangleVertices :: IO (GL.VertexArrayObject, GL.BufferObject)
createTriangleVertices = do
  let vertices = V.fromList
        [ 0.0, 0.5, 0.0
        , -0.5, -0.5, 0.0
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

gameLoop :: Window -> GL.VertexArrayObject -> GameScene -> Integer -> GL.Program -> IO ()
gameLoop window triangleVao gameScene prevTime program = do
  events <- pollEvents
  currentUnixTime <- getUnixTimeMillis

  let quit = any (isQuitEvent . eventPayload) events
      timeDelta = currentUnixTime - prevTime

  -- Clear the screen
  GL.clearColor $= GL.Color4 0 0 0 1
  GL.clear [GL.ColorBuffer]

  let playerBall = head (balls $ _gameScene_actors gameScene)
      otherBall = moveActor $ balls (_gameScene_actors gameScene) !! 1
      dummyPaddle = head (paddles $ _gameScene_actors gameScene)
      collisionOccurred = collides playerBall otherBall
      collidedBall = if collisionOccurred then applyCollision playerBall else playerBall
      newBall = moveActor collidedBall 

  -- Log collision status
  putStrLn $ "Frame at " ++ show currentUnixTime ++ "ms: Collision " ++ if collisionOccurred then "detected" else "not detected"

  (ballVao, ballVbo, ballNumVertices) <- createActorVertices newBall
  (ball2Vao, ball2Vbo, ball2NumVertices) <- createActorVertices otherBall
  (paddleVao, paddleVbo, paddleNumVertices) <- createActorVertices dummyPaddle 

  GL.currentProgram $= Just program
  GL.bindVertexArrayObject $= Just ballVao
  GL.drawArrays GL.TriangleFan 0 (fromIntegral ballNumVertices)

  GL.bindVertexArrayObject $= Just ball2Vao
  GL.drawArrays GL.TriangleFan 0 (fromIntegral ball2NumVertices)

  GL.bindVertexArrayObject $= Just paddleVao
  GL.drawArrays GL.TriangleFan 0 (fromIntegral paddleNumVertices)

  -- Swap buffers
  glSwapWindow window

  let updatedGameScene = GameScene (Actors [newBall, otherBall] [dummyPaddle]) (_gameScene_boundary gameScene)
  unless quit $ gameLoop window triangleVao updatedGameScene currentUnixTime program
