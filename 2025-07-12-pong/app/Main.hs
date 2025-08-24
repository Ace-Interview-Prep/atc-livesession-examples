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

isInBoundary :: Actor -> Boundary -> Bool
isInBoundary (Ball pos size _) boundary
  = ((getX pos) - size < _boundary_left boundary)
  || ((getX pos) + size > _boundary_right boundary)
  || ((getY pos) - size < _boundary_top boundary)
  || ((getY pos) + size > _boundary_bottom boundary)
isInBoundary (Paddle pos width height) boundary
  = ((getX pos) - width < _boundary_left boundary)
  || ((getX pos) + width > _boundary_right boundary)
  || ((getY pos) - height < _boundary_top boundary)
  || ((getY pos) + height > _boundary_bottom boundary)

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

  let playerBall = Ball (V2 0 0) 0.1 (V2 0.0 (-0.0005))
      otherBall = Ball (V2 0 (-0.5)) 0.1 (V2 0.0 0.0)
      boundary = Boundary 0.0 0.0 0.0 0.0
      gameScene = GameScene [playerBall, otherBall] boundary

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

createActorVertices :: Actor -> IO (GL.VertexArrayObject, GL.BufferObject, Int)
createActorVertices (Ball ballPos size _) = do
  let V2 ballX ballY = ballPos
      ballRadius = size / 2
      vertices
        = V.fromList
        $ concat [ [ballX, ballY, 0.0]
                 ] ++ unwrapV2 (createCircleVertices ballPos ballRadius)
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
createActorVertices (Paddle pos width height) = do
  let V2 rectX rectY = pos
      V2 rectW rectH = V2 width height

      vertices = V.fromList $
        concatMap (\(V2 x y) -> [x, y, 0.0]) $
        [ V2 rectX         rectY
        , V2 (rectX+rectW) rectY
        , V2 rectX         (rectY+rectH)
        , V2 rectX         (rectY+rectH)
        , V2 (rectX+rectW) rectY
        , V2 (rectX+rectW) (rectY+rectH)
        ]

      numVertices = V.length vertices `div` 3

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

createCircleVertices :: V2 Float -> Float -> [V2 Float]
createCircleVertices pos radius = (calculateCircumferencePoint pos radius) <$> [1..360]

calculateCircumferencePoint :: V2 Float -> Float -> Int -> V2 Float
calculateCircumferencePoint pos r th = V2 (r * (sin (fromIntegral th))) (r * (cos (fromIntegral th))) + pos

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

-- hasCollided :: Actor -> [Actor] -> Bool
-- hasCollided _ [] = False
-- hasCollided (Ball pos size mv) ((Ball pos2 size2 _):bs) =
--   let
--     distanceX = (getX pos) - (getX pos2)
--     distanceY = (getY pos) - (getY pos2)
--     distance = sqrt ((distanceX ** 2) + (distanceY ** 2))
--   in
--     if distance > (size/2) + (size2/2)
--     then hasCollided (Ball pos size mv) bs
--     else True
-- hasCollided _ _ = False

applyCollision :: Actor -> Actor
applyCollision (Ball pos size mv) = Ball pos size (-mv)
applyCollision (Paddle pos width height) = Paddle pos width height

moveActor :: Actor -> GameScene -> Actor
moveActor (Ball pos size mv) scene = Ball (pos + mv) size mv
moveActor (Paddle pos width height) scene = Paddle pos width height

gameLoop :: Window -> GL.VertexArrayObject -> GameScene -> Integer -> GL.Program -> IO ()
gameLoop window triangleVao gameScene prevTime program = do
  events <- pollEvents
  currentUnixTime <- getUnixTimeMillis

  let quit = any (isQuitEvent . eventPayload) events
      timeDelta = currentUnixTime - prevTime

  -- Clear the screen
  GL.clearColor $= GL.Color4 0 0 0 1
  GL.clear [GL.ColorBuffer]

  let playerBall = (_gameScene_actors gameScene) !! 0
      otherBall = moveActor ((_gameScene_actors gameScene) !! 1) gameScene
      collisionOccurred = hasCollided playerBall [otherBall]
      collidedBall = if collisionOccurred then applyCollision playerBall else playerBall
      newBall = moveActor collidedBall gameScene

  -- Log collision status
  putStrLn $ "Frame at " ++ show currentUnixTime ++ "ms: Collision " ++ if collisionOccurred then "detected" else "not detected"

  (ballVao, ballVbo, ballNumVertices) <- createActorVertices newBall
  (ball2Vao, ball2Vbo, ball2NumVertices) <- createActorVertices otherBall

  GL.currentProgram $= Just program
  GL.bindVertexArrayObject $= Just ballVao
  GL.drawArrays GL.TriangleFan 0 (fromIntegral ballNumVertices)

  GL.bindVertexArrayObject $= Just ball2Vao
  GL.drawArrays GL.TriangleFan 0 (fromIntegral ball2NumVertices)

  -- Swap buffers
  glSwapWindow window

  let updatedGameScene = GameScene [newBall, otherBall] (_gameScene_boundary gameScene)
  unless quit $ gameLoop window triangleVao updatedGameScene currentUnixTime program
