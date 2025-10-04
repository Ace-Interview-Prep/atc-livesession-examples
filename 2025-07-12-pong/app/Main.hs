{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}

module Main where

import Control.Monad (unless)
import Data.IORef
import Data.Time.Clock.POSIX (getPOSIXTime)
import Graphics.GPipe
import qualified Graphics.GPipe.Context.GLFW as GLFW

import Rendering
import Ball

screenW, screenH :: Int
screenW = 800
screenH = 800

getUnixTimeMillis :: IO Integer
getUnixTimeMillis = floor . (*1000) <$> getPOSIXTime



data GameState = GameState
  { _gameState_actors :: [Ball]
  }



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

calculateBounceVelocity :: Ball -> Ball -> V2 Float
calculateBounceVelocity (Ball ballPos1 _ ballVel1 _) (Ball ballPos2 _ ballVel2 _) =
  let
    collisionVector   = ballPos2 - ballPos1
    normalVector      = normalizeV2 collisionVector
    relativeVelocity  = ballVel1 - ballVel2
    dotProduct        = (getX relativeVelocity) * (getX normalVector) + (getY relativeVelocity) * (getY normalVector)
    velocityChange    = scalarMultiply normalVector dotProduct
  in
    ballVel1 - velocityChange


hasCollided :: Ball -> [Ball] -> Bool
hasCollided _ [] = False
hasCollided (Ball pos size mv label) ((Ball pos2 size2 _ _):bs) =
  let
    distanceX = (getX pos) - (getX pos2)
    distanceY = (getY pos) - (getY pos2)
    distance = sqrt ((distanceX ** 2) + (distanceY ** 2))
  in
    if distance > (size/2) + (size2/2)
    then hasCollided (Ball pos size mv label) bs
    else True
hasCollided _ _ = False


applyCollision :: Ball -> Ball -> Ball
applyCollision ball1@(Ball pos size mv label1) ball2@(Ball pos2 size2 mv2 label2) =
  let newVelocity = calculateBounceVelocity ball1 ball2
  in Ball pos size newVelocity label1

moveBall :: Ball -> GameState -> Float -> Ball
moveBall (Ball pos size (V2 velX velY) label) state timeDelta =
  let movementVector = V2 (velX * timeDelta) (velY * timeDelta)
  in Ball (pos + movementVector) size (V2 velX velY) label


runGameStep :: GameState -> Float -> GameState
runGameStep gameState timeDelta =
  let playerBall = (_gameState_actors gameState) !! 0
      otherBall = (_gameState_actors gameState) !! 1
      collisionOccurred = hasCollided playerBall [otherBall]
      collidedBall = if collisionOccurred then applyCollision playerBall otherBall else playerBall
      collidedBall' = if collisionOccurred then applyCollision otherBall playerBall else otherBall
      newBall = moveBall collidedBall gameState timeDelta
      newBall2 = moveBall collidedBall' gameState timeDelta
  in
    GameState [newBall, newBall2] (_gameState_boundary gameState)

initialGameState :: GameState
initialGameState =
  let playerVel = V2 0.0 0.1
      otherVel = V2 0.0 (-0.1)
      playerSize = 0.1
      otherSize = 0.1
      playerStartPos = V2 0 0
      otherStartPos = V2 0 (-0.5)

      playerBall = Ball playerStartPos playerSize playerVel  "Player"
      otherBall  = Ball otherStartPos  otherSize  otherVel   "Other"
  in
      GameState [playerBall, otherBall]

buildInitialScene :: Renderer os -> GameState -> ContextT GLFW.Handle os IO (Scene os)
buildInitialScene renderer gameState = do
  let playerBall = (_gameState_actors gameState) !! 0
      otherBall = (_gameState_actors gameState) !! 1
      circleVerts = mkCircleVerts (playerSize / 2) 64

  playerR <- createRenderableFromCircle renderer circleVerts (_ball_pos playerBall)
  otherR <- createRenderableFromCircle renderer circleVerts (_ball_pos otherBall)

  pure $ Scene [playerR, otherR]

updateScene :: Scene os -> GameState -> Scene os
updateScene scene gs =
  let actor1 = (_gameState_actors gs) !! 0
      actor2 = (_gameState_actors gs) !! 1
      r1 = (_renderables scene) !! 0
      r2 = (_renderables scene) !! 1
  in Scene [ r1 { origin = _ball_pos actor1 }
           , r2 { origin = _ball_pos actor2 }
           ]

gameLoop :: Renderer os -> Scene os -> GameState -> Integer -> ContextT GLFW.Handle os IO ()
gameLoop renderer scene gameState prevTime = do
  GLFW.pollEvents
  shouldClose <- GLFW.windowShouldClose (win renderer)

  currentTime <- liftIO getUnixTimeMillis
  let timeDelta = realToFrac (currentTime - prevTime) / 1000.0 :: Float
      updatedGameState = runGameStep gameState timeDelta
      updatedScene = updateScene scene updatedGameState

  renderScene
    renderer
    updatedScene

  unless shouldClose $
    gameLoop
      renderer
      updatedScene
      updatedGameState
      currentTime


main :: IO ()
main = runContextT GLFW.defaultHandleConfig $ do
  renderer <- initRenderer (V2 screenW screenH)
  initialScene <- buildInitialScene renderer
  startTime <- liftIO getUnitTimeMillis

  gameLoop
    renderer
    initialScene
    initialGameState
    startTime

  destroyAll renderer





















































-- import SDL hiding (createWindow, origin)
-- import SDL.Video.OpenGL (glCreateContext)
-- import Prelude hiding (log)
-- import Foreign.Ptr
-- import Foreign.C.Types (CInt)
-- import Control.Exception (SomeException, try, throwIO)
-- import Control.Monad (unless, void)
-- import Data.ByteString (ByteString)
-- import GHC.Int
-- import qualified Graphics.Rendering.OpenGL as GL
-- import qualified Data.Vector.Storable as V
-- import qualified Debug.Trace as DT
-- import Graphics.Rendering.OpenGL.GL.CoordTrans
-- import Shaders (vertexShaderSource, fragmentShaderSource, createShaderProgram)
-- import Window (createWindow)
-- import Data.Time.Clock.POSIX (getPOSIXTime)
-- import qualified Linear.V2 as L
-- import Data.IORef

-- import Debug.Trace as DT

-- import Ball
-- import Rendering
-- import qualified Rendering as R

-- gravity :: Gravity
-- gravity = Gravity 9.80665

-- screenWidth :: (Num a) => a
-- screenWidth = 800

-- screenHeight :: (Num a) => a
-- screenHeight = 800

-- windowConfig :: WindowConfig
-- windowConfig = defaultWindow
--                { windowInitialSize = V2 screenWidth screenHeight
--                , windowGraphicsContext = OpenGLContext defaultOpenGL
--                                          { glProfile = Core Normal 3 3
--                                          }
--                }

-- main :: IO ()
-- main = do
--   SDL.initializeAll
--   window <- createWindow windowConfig "Ping Pong"
--   glContext <- createGLContext window
--   setViewport (Position 0 0) (Size screenWidth screenHeight)
--   program <- createShaderProgram
--   setShaderProgram program

--   let playerVelocity = V2 0.0 (0.1)
--       otherBallVelocity = V2 0.0 (-0.1)
--       playerSize = 0.1
--       otherBallSize = 0.1
--       playerStartPos = V2 0 0
--       otherBallStartPos = V2 0 (-0.5)

--       playerBall = Ball playerStartPos playerSize playerVelocity "Player"
--       otherBall = Ball otherBallStartPos otherBallSize otherBallVelocity "Other"
--       boundary = Boundary 0.0 0.0 0.0 0.0
--       gameState = GameState [playerBall, otherBall] boundary
--       scene = Scene []

--   -- Create initial renderables
--   playerBallR <- createRenderable playerBall
--   otherBallR <- createRenderable otherBall

--   let scene = Scene [ playerBallR
--                     , otherBallR
--                     ]

--   modelLoc <- GL.get $ GL.uniformLocation program "uModel"
--   let render = renderScene window program modelLoc

--   currentUnixTime <- getUnixTimeMillis

--   finalScene <- gameLoop render scene gameState currentUnixTime
--   let vaos = vao <$> (_renderables finalScene)
--       vbos = vbo <$> (_renderables finalScene)

--   R.destroyAll window glContext [program] vaos vbos
--   quit

-- isQuitEvent :: EventPayload -> Bool
-- isQuitEvent e = case e of
--   QuitEvent -> True
--   KeyboardEvent keyboardEvent ->
--     keyboardEventKeyMotion keyboardEvent == Pressed &&
--     keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeEscape
--   _ -> False




-- runRenderScene :: R.Scene -> GameState -> R.Scene
-- runRenderScene scene gameState =
--   let newBall = (_gameState_actors gameState) !! 0
--       newBall2 = (_gameState_actors gameState) !! 1
--       updatedPlayerBallR = ((_renderables scene) !! 0) { origin = _ball_pos newBall }
--       updatedOtherBallR = ((_renderables scene) !! 1) { origin = _ball_pos newBall2 }
--   in
--     R.Scene [updatedPlayerBallR, updatedOtherBallR]


-- gameLoop :: (R.Scene -> IO ()) -> R.Scene -> GameState -> Integer -> IO (R.Scene)
-- gameLoop render scene gameState prevTime = do
--   events <- pollEvents
--   currentUnixTime <- getUnixTimeMillis

--   let quit = any (isQuitEvent . eventPayload) events
--       timeDelta = (fromInteger (currentUnixTime - prevTime)) / 1000

--   -- Run game step
--   case quit of
--     False -> do
--       let updatedGameState = runGameStep gameState timeDelta
--           updatedScene = runRenderScene scene updatedGameState

--       render updatedScene
--       gameLoop render updatedScene updatedGameState currentUnixTime

--     True ->
--       return scene


-- getX :: V2 a -> a
-- getX (V2 x _) = x

-- getY :: V2 a -> a
-- getY (V2 _ y) = y

-- getUnixTimeMillis :: IO Integer
-- getUnixTimeMillis = do
--   time <- getPOSIXTime
--   return $ floor (time * 1000)

-- data Boundary = Boundary
--   { _boundary_left :: Float
--   , _boundary_right :: Float
--   , _boundary_top :: Float
--   , _boundary_bottom :: Float
--   }

-- isInBoundary :: Ball -> Boundary -> Bool
-- isInBoundary (Ball pos size _ _) boundary
--   = ((getX pos) - size < _boundary_left boundary)
--   || ((getX pos) + size > _boundary_right boundary)
--   || ((getY pos) - size < _boundary_top boundary)
--   || ((getY pos) + size > _boundary_bottom boundary)

-- newtype Mass = Mass Float
-- newtype Gravity = Gravity Float

-- data GameState = GameState
--   { _gameState_actors :: [Ball]
--   , _gameState_boundary :: Boundary
--   }
