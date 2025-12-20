{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad (unless)
import Control.Monad.IO.Class
import Data.Maybe
import Data.Time.Clock.POSIX (getPOSIXTime)
import Graphics.GPipe hiding (distance)
import qualified Graphics.GPipe.Context.GLFW as GLFW

import Rendering
import Ball
import Paddle

screenW, screenH :: Int
screenW = 800
screenH = 800

getUnixTimeMillis :: IO Integer
getUnixTimeMillis = floor . (*1000) <$> getPOSIXTime


paddleMovementSpeed :: Float
paddleMovementSpeed = 0.1


data Actor = forall a. (Renderable a, RespondsToInput a) => Actor a
instance Renderable Actor where
  createRenderable renderer (Actor actor) = createRenderable renderer actor

instance RespondsToInput Actor where
  move input timeDelta (Actor actor) = Actor $ move input timeDelta actor


class RespondsToInput a where
  move :: InputKeyMap -> TimeDelta -> a -> a


data GameState = GameState
  { _gameState_actors :: [Actor]
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


-- runGameStep :: GameState -> Float -> GameState
-- runGameStep gameState timeDelta =
--   let playerPaddle = (_gameState_actors gameState) !! 0
--       gameBall = (_gameState_actors gameState) !! 1
--       collisionOccurred = hasCollided playerPaddle [gameBall]
--       --collidedBall = if collisionOccurred then applyCollision playerBall otherBall else playerBall
--       --collidedBall' = if collisionOccurred then applyCollision otherBall playerBall else otherBall
--       --newBall = moveBall collidedBall timeDelta
--       --newBall2 = moveBall collidedBall' timeDelta
--   in
--     --GameState [newBall, newBall2]
--     GameState [playerPaddle, gameBall]


initialGameState :: GameState
initialGameState =
  let playerStartPos = V2 (-1) 0
      paddleWidth = 0.05
      paddleHeight = 0.4
      ballStartPos = V2 0.5 0.5
      ballStartVel = V2 0.0 (-0.1)
      ballSize = 0.1

      playerPaddle = Actor $ Paddle playerStartPos paddleWidth paddleHeight "Player"
      gameBall = Actor $ Ball ballStartPos ballSize ballStartVel "Ball"
  in
      GameState [gameBall, playerPaddle]


buildInitialScene :: Renderer os -> GameState -> ContextT GLFW.Handle os IO (Scene os)
buildInitialScene renderer (GameState actors) = do
  renderables <- mapM (createRenderable renderer) actors
  pure $ Scene renderables

  -- let playerPaddle = (_gameState_actors gameState) !! 0
  --     gameBall = (_gameState_actors gameState) !! 1

  -- playerR <- createRenderable renderer playerPaddle
  -- ballR <- createRenderable renderer gameBall

  -- pure $ Scene [playerR, ballR]



-- updateScene :: Scene os -> GameState -> Scene os
-- updateScene scene gs =
--   let actor1 = (_gameState_actors gs) !! 0
--       actor2 = (_gameState_actors gs) !! 1
--       r1 = (_renderables scene) !! 0
--       r2 = (_renderables scene) !! 1
--   in Scene [ r1 { origin = _ball_pos actor1 }
--            , r2 { origin = _paddle_pos actor2 }
--            ]


type KeyState = Int
data InputKeyMap = InputKeyMap
  { _inputKey_w :: KeyState
  , _inputKey_s :: KeyState
  }

newtype TimeDelta = TimeDelta Float

mapKeyState :: Maybe GLFW.KeyState -> KeyState
mapKeyState = \case
  Just GLFW.KeyState'Pressed -> 1
  Just GLFW.KeyState'Released -> 0
  Just GLFW.KeyState'Repeating -> 0
  Nothing -> 0


instance RespondsToInput Ball where
  move _ _ ball = ball


instance RespondsToInput Paddle where
  move input (TimeDelta timeDelta) paddle =
    let upwardMovement = (fromIntegral $ _inputKey_w input) * timeDelta * 1
        downwardMovement = (fromIntegral $ _inputKey_s input) * timeDelta * (-1)
        movementDelta = upwardMovement + downwardMovement
        (V2 paddleX paddleY) = _paddle_pos paddle
    in paddle
       { _paddle_pos = V2 paddleX (paddleY + movementDelta)
       }


createScene :: GameState -> Scene
createScene gameState = ??


gameLoop :: Renderer os -> Scene os -> GameState -> Integer -> ContextT GLFW.Handle os IO ()
gameLoop renderer scene gameState prevTime = do
  let window = win renderer
  shouldClose <- fromMaybe False <$> GLFW.windowShouldClose window

  currentTime <- liftIO getUnixTimeMillis
  let timeDelta = TimeDelta $ realToFrac (currentTime - prevTime) / 1000.0
      --updatedGameState = runGameStep gameState timeDelta
      --updatedGameState = gameState
      --updatedScene = updateScene scene updatedGameState
      updatedScene = scene

  keyW <- GLFW.getKey window GLFW.Key'W
  keyS <- GLFW.getKey window GLFW.Key'S

  let updatedInputMap = InputKeyMap
        { _inputKey_w = mapKeyState keyW
        , _inputKey_s = mapKeyState keyS
        }

  -- move this stuff to game function code
  -- let padd = _gameState_actors gameState !! 1
  --     newPadd = movePaddle updatedInputMap padd timeDelta

  let updatedGameState = gameState
        { _gameState_actors = move updatedInputMap timeDelta <$> _gameState_actors gameState
        }
      updatedScene = scene updatedGameState

  -- call move paddle

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
