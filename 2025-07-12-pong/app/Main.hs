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

getX :: V2 a -> a
getX (V2 x _) = x

getY :: V2 a -> a
getY (V2 _ y) = y

getUnixTimeMillis :: IO Integer
getUnixTimeMillis = do
  time <- getPOSIXTime
  return $ floor (time * 1000)

data Ball
  = Ball
    { _ball_pos :: V2 Float
    , _ball_size :: Float
    , _ball_velocity :: V2 Float
    }
  -- | Paddle
  --   { _paddle_pos :: V2 Float
  --   , _paddle_width :: Float
  --   , _paddle_height :: Float
  --   }

data Boundary = Boundary
  { _boundary_left :: Float
  , _boundary_right :: Float
  , _boundary_top :: Float
  , _boundary_bottom :: Float
  }

isInBoundary :: Ball -> Boundary -> Bool
isInBoundary (Ball pos size _) boundary
  = ((getX pos) - size < _boundary_left boundary)
  || ((getX pos) + size > _boundary_right boundary)
  || ((getY pos) - size < _boundary_top boundary)
  || ((getY pos) + size > _boundary_bottom boundary)
-- isInBoundary (Paddle pos width height) boundary
--   = ((getX pos) - width < _boundary_left boundary)
--   || ((getX pos) + width > _boundary_right boundary)
--   || ((getY pos) - height < _boundary_top boundary)
--   || ((getY pos) + height > _boundary_bottom boundary)

newtype Mass = Mass Float
newtype Gravity = Gravity Float

data GameScene = GameScene
  { _gameScene_actors :: [Ball]
  , _gameScene_boundary :: Boundary
  }

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

  let playerVelocity = V2 0.0 (-0.5)
      otherBallVelocity = V2 0.0 0.0
      playerSize = 0.1
      otherBallSize = 0.1
      playerStartPos = V2 0 0
      otherBallStartPos = V2 0 (-0.5)

      playerBall = Ball playerStartPos playerSize playerVelocity
      otherBall = Ball otherBallStartPos otherBallSize otherBallVelocity
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


absV2 :: V2 Float -> V2 Float
absV2 (V2 x y) = V2 (abs x) (abs y)

perpendicularV2 :: V2 Float -> V2 Float
perpendicularV2 (V2 x y) = V2 y (-x)

normalizeV2 :: V2 Float -> V2 Float
normalizeV2 v =
  let magnitude = sqrt (((getX v) ** 2) + (getY v) ** 2)
  in V2 (getX v / magnitude) (getY v / magnitude)

scalarMultiply :: V2 Float -> Float -> V2 Float
scalarMultiply (V2 vx vy) s = V2 (vx * s) (vy * s)


-- TODO: Create these functions:
-- calculateBounce -- multiply velocity by dot product, assume 1:1 mass relationship
calculateBounceVelocity :: Ball -> Ball -> V2 Float
calculateBounceVelocity (Ball ballPos1 _ ballVel1) (Ball ballPos2 _ ballVel2) =
  let collisionVector = ballPos2 - ballPos1
      perpendicularVector = perpendicularV2 collisionVector
      normalVector = normalizeV2 perpendicularVector
      dotProduct = (getX ballVel1) * (getX normalVector) + (getY ballVel1) * (getY normalVector)
  in
    ballVel1 - (scalarMultiply normalVector (2 * dotProduct))


--calculateBounce actor1 actor2 =

createBallVertices :: Ball -> IO (GL.VertexArrayObject, GL.BufferObject, Int)
createBallVertices (Ball ballPos size _) = do
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

hasCollided :: Ball -> [Ball] -> Bool
hasCollided _ [] = False
hasCollided (Ball pos size mv) ((Ball pos2 size2 _):bs) =
  let
    distanceX = (getX pos) - (getX pos2)
    distanceY = (getY pos) - (getY pos2)
    distance = sqrt ((distanceX ** 2) + (distanceY ** 2))
  in
    if distance > (size/2) + (size2/2)
    then hasCollided (Ball pos size mv) bs
    else True
hasCollided _ _ = False

applyCollision :: Ball -> Ball -> Ball
applyCollision ball1@(Ball pos size mv) ball2@(Ball pos2 size2 mv2) =
  let newVelocity = calculateBounceVelocity ball1 ball2
  in Ball pos size newVelocity

moveBall :: Ball -> GameScene -> Float -> Ball
moveBall (Ball pos size (V2 velX velY)) scene timeDelta =
  let movementVector = V2 (velX * timeDelta) (velY * timeDelta)
  in Ball (pos + movementVector) size (V2 velX velY)

gameLoop :: Window -> GL.VertexArrayObject -> GameScene -> Integer -> GL.Program -> IO ()
gameLoop window triangleVao gameScene prevTime program = do
  events <- pollEvents
  currentUnixTime <- getUnixTimeMillis

  let quit = any (isQuitEvent . eventPayload) events
      timeDelta = (fromInteger (currentUnixTime - prevTime)) / 1000

  --putStrLn $ "Time Delta " ++ show timeDelta

  -- Clear the screen
  GL.clearColor $= GL.Color4 0 0 0 1
  GL.clear [GL.ColorBuffer]

  let playerBall = (_gameScene_actors gameScene) !! 0
      otherBall = moveBall ((_gameScene_actors gameScene) !! 1) gameScene timeDelta
      collisionOccurred = hasCollided playerBall [otherBall]
      collidedBall = if collisionOccurred then applyCollision playerBall otherBall else playerBall
      newBall = moveBall collidedBall gameScene timeDelta

  -- Log collision status
  putStrLn $ "Frame at " ++ show currentUnixTime ++ "ms: Collision " ++ if collisionOccurred then "detected" else "not detected"
  putStrLn $ "calculateBounceVelocity ball1 ball2" ++ show (calculateBounceVelocity playerBall otherBall)


  (ballVao, ballVbo, ballNumVertices) <- createBallVertices newBall
  (ball2Vao, ball2Vbo, ball2NumVertices) <- createBallVertices otherBall

  GL.currentProgram $= Just program
  GL.bindVertexArrayObject $= Just ballVao
  GL.drawArrays GL.TriangleFan 0 (fromIntegral ballNumVertices)

  GL.bindVertexArrayObject $= Just ball2Vao
  GL.drawArrays GL.TriangleFan 0 (fromIntegral ball2NumVertices)

  -- Swap buffers
  glSwapWindow window

  let updatedGameScene = GameScene [newBall, otherBall] (_gameScene_boundary gameScene)
  unless quit $ gameLoop window triangleVao updatedGameScene currentUnixTime program
