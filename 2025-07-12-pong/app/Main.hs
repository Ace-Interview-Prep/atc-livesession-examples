{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}

module Main where

import Control.Monad (unless)
import Control.Monad.IO.Class
import Data.Maybe
import Data.Time.Clock.POSIX (getPOSIXTime)
import Graphics.GPipe hiding (distance)
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

getX :: V2 a -> a
getX (V2 x _) = x

getY :: V2 a -> a
getY (V2 _ y) = y

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


applyCollision :: Ball -> Ball -> Ball
applyCollision ball1@(Ball pos size _ label) ball2 =
  let newVelocity = calculateBounceVelocity ball1 ball2
  in Ball pos size newVelocity label

moveBall :: Ball -> Float -> Ball
moveBall (Ball pos size (V2 velX velY) label) timeDelta =
  let movementVector = V2 (velX * timeDelta) (velY * timeDelta)
  in Ball (pos + movementVector) size (V2 velX velY) label


runGameStep :: GameState -> Float -> GameState
runGameStep gameState timeDelta =
  let playerBall = (_gameState_actors gameState) !! 0
      otherBall = (_gameState_actors gameState) !! 1
      collisionOccurred = hasCollided playerBall [otherBall]
      collidedBall = if collisionOccurred then applyCollision playerBall otherBall else playerBall
      collidedBall' = if collisionOccurred then applyCollision otherBall playerBall else otherBall
      newBall = moveBall collidedBall timeDelta
      newBall2 = moveBall collidedBall' timeDelta
  in
    GameState [newBall, newBall2]


initialGameState :: GameState
initialGameState =
  let playerVel = V2 0.0 (-0.1)
      otherVel = V2 0.0 (0.1)
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
      playerVerts = mkCircleVerts ((_ball_size playerBall) / 2) 64
      otherVerts = mkCircleVerts ((_ball_size otherBall) / 2) 64

  playerR <- createVertexBufferFromCircle renderer playerVerts (_ball_pos playerBall)
  otherR <- createVertexBufferFromCircle renderer otherVerts (_ball_pos otherBall)

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
  shouldClose <- fromMaybe False <$> GLFW.windowShouldClose (win renderer)

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
  initialScene <- buildInitialScene renderer initialGameState
  startTime <- liftIO getUnixTimeMillis

  gameLoop
    renderer
    initialScene
    initialGameState
    startTime

  destroyAll renderer
