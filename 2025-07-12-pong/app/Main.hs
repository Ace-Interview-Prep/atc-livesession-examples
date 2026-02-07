{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad (unless)
import Control.Monad.IO.Class
import Data.List (find)
import Data.Ord (clamp)
import Data.Maybe
import Data.Time.Clock.POSIX (getPOSIXTime)
import Graphics.GPipe hiding (distance, clamp)
import qualified Graphics.GPipe.Context.GLFW as GLFW
import qualified Debug.Trace as DT

import Rendering
import Ball
import Paddle

screenW, screenH :: Int
screenW = 800
screenH = 800

getUnixTimeMillis :: IO Integer
getUnixTimeMillis = floor . (*1000) <$> getPOSIXTime


paddleMovementSpeed :: Float
paddleMovementSpeed = 0.2


data Actor = forall a. (Renderable a) => Actor a

instance Renderable Actor where
  createRenderable renderer (Actor actor) = createRenderable renderer actor

instance Renderable Player where
  createRenderable renderer player = createRenderable renderer (_player_paddle player)

class RespondsToInput a where
  move :: TimeDelta -> a -> a -- Where did the input map go?

data Player = Player
  { _player_name :: PlayerName
  , _player_input :: PlayerInput
  , _player_paddle :: Paddle
  }

type PlayerName = String

data PlayerInput = PlayerInput
  { _playerInput_up :: KeyState
  , _playerInput_down :: KeyState
  }

defaultPlayerInput = PlayerInput 0 0

data GameState = GameState
  { _gameState_player1 :: Player
  , _gameState_player2 :: Player
  , _gameState_ball :: Ball
  , _gameState_gizmoBallA :: Ball
  , _gameState_gizmoBallB :: Ball
  }

initialGameState :: GameState
initialGameState =
  let player1StartPos = V2 (-0.8) 0
      player2StartPos = V2 (0.8) 0
      paddleWidth = 0.05
      paddleHeight = 0.4
      ballStartPos = V2 0.5 0
      ballStartVel = V2 (-0.8) 0
      ballSize = 0.1

      gameBall = Ball ballStartPos ballSize ballStartVel "Ball" (V4 1 1 1 1)
      gizmoBallA = Ball player1StartPos paddleWidth (V2 0 0) "Player1 Gizmo" (V4 0 1 0 0)
      gizmoBallB = Ball player2StartPos paddleWidth (V2 0 0) "Player2 Gizmo" (V4 0 1 0 0)

      player1Paddle = Paddle player1StartPos paddleWidth paddleHeight "Player1"
      player2Paddle = Paddle player2StartPos paddleWidth paddleHeight "Player2"
      player1 = Player "Player 1" defaultPlayerInput player1Paddle
      player2 = Player "Player 2" defaultPlayerInput player2Paddle

  in
      GameState player1 player2 gameBall gizmoBallA gizmoBallB


main :: IO ()
main = runContextT GLFW.defaultHandleConfig $ do
  renderer <- initRenderer (V2 screenW screenH)
  initialScene <- buildScene renderer initialGameState
  startTime <- liftIO getUnixTimeMillis

  gameLoop
    renderer
    initialScene
    initialGameState
    startTime

  destroyAll renderer


gameLoop :: Renderer os -> Scene os -> GameState -> Integer -> ContextT GLFW.Handle os IO ()
gameLoop renderer scene gameState prevTime = do
  let window = win renderer
  shouldClose <- fromMaybe False <$> GLFW.windowShouldClose window

  currentTime <- liftIO getUnixTimeMillis
  let timeDelta = TimeDelta $ realToFrac (currentTime - prevTime) / 1000.0

  keyW <- GLFW.getKey window GLFW.Key'W
  keyS <- GLFW.getKey window GLFW.Key'S
  keyUp <- GLFW.getKey window GLFW.Key'Up
  keyDown <- GLFW.getKey window GLFW.Key'Down

  let updatedInputMap = InputKeyMap
                        { _inputKey_w = mapKeyState keyW
                        , _inputKey_s = mapKeyState keyS
                        , _inputKey_up = mapKeyState keyUp
                        , _inputKey_down = mapKeyState keyDown
                        }

      player1Input = PlayerInput
                     { _playerInput_up = _inputKey_w updatedInputMap
                     , _playerInput_down = _inputKey_s updatedInputMap
                     }

      player2Input = PlayerInput
                     { _playerInput_up = _inputKey_up updatedInputMap
                     , _playerInput_down = _inputKey_down updatedInputMap
                     }

      player1 = _gameState_player1 gameState
      updatedPlayer1 = player1
                       { _player_input = player1Input
                       }

      player2 = _gameState_player2 gameState
      updatedPlayer2 = player2
                       { _player_input = player2Input
                       }

  let newBall = moveBall (handleCollision gameState) timeDelta
      newPlayer1 = move timeDelta updatedPlayer1
      newPlayer2 = move timeDelta updatedPlayer2

  let updatedGameState = gameState
                         { _gameState_player1 = newPlayer1
                         , _gameState_player2 = newPlayer2
                         --, _gameState_environment = _gameState_environment gameState
                         , _gameState_ball = newBall
                         , _gameState_gizmoBallA = updateGizmoBall (_player_paddle newPlayer1) (_gameState_gizmoBallA gameState) (_ball_pos newBall)
                         , _gameState_gizmoBallB = updateGizmoBall (_player_paddle newPlayer2) (_gameState_gizmoBallB gameState) (_ball_pos newBall)
                         --_gameState_actors = move updatedInputMap timeDelta <$> _gameState_actors gameState
                         }

  updatedScene <- buildScene renderer updatedGameState

  renderScene
    renderer
    updatedScene

  unless shouldClose $
    gameLoop
      renderer
      updatedScene
      updatedGameState
      currentTime


updateGizmoBall :: Paddle -> Ball -> V2 Float -> Ball
updateGizmoBall (Paddle pPos pWidth pHeight pLabel) ball pos =
  let
    circleColliderX = ((getX pPos) + (pWidth / 2))
    circleColliderY = clamp ((getY pPos) - (pHeight / 2), (getY pPos) + (pHeight / 2)) $ getY pos
  in
    ball { _ball_pos = V2 circleColliderX circleColliderY
         , _ball_size = pWidth
         }

calculateBounceVelocity :: Ball -> Ball -> V2 Float
calculateBounceVelocity (Ball ballPos1 _ ballVel1 _ _) (Ball ballPos2 _ ballVel2 _ _) =
  let
    collisionVector   = ballPos2 - ballPos1
    normalVector      = normalizeV2 collisionVector
    relativeVelocity  = ballVel1 - ballVel2
    dotProduct        = (getX relativeVelocity) * (getX normalVector) + (getY relativeVelocity) * (getY normalVector)
    velocityChange    = scalarMultiply normalVector dotProduct
  in
    ballVel1 - velocityChange


hasCollided :: Ball -> Paddle -> Bool
hasCollided ball@(Ball bPos bSize bMv bLabel _) (Paddle pPos pWidth pHeight pLabel) =
  let
    circleColliderX = ((getX pPos) + (pWidth / 2))
    circleColliderY = clamp ((getY pPos) - (pHeight / 2), (getY pPos) + (pHeight / 2)) $ getY bPos
    distanceX = (getX bPos) - circleColliderX
    distanceY = (getY bPos) - circleColliderY
    distance = sqrt ((distanceX ** 2) + distanceY ** 2)
  in
    distance <= (bSize / 2) + (pWidth / 2)


calculateOverlap :: Ball -> Ball -> V2 Float
calculateOverlap ball1@(Ball bp1 bs1 _ _ _) ball2@(Ball bp2 bs2 _ _ _) =
  let
    distance = lengthV2 (bp1 - bp2)
    direction = normalizeV2 (bp1 - bp2)
    overlapAmount = ((bs1 / 2) + (bs2 / 2)) - distance
  in
    scalarMultiply direction overlapAmount



hasBallCollidedWithBoundary :: Ball -> Float -> Bool
hasBallCollidedWithBoundary ball boundaryPosY =
  let
    distanceY = abs $ (getY $ _ball_pos ball) - boundaryPosY
  in
    distanceY <= (_ball_size ball / 2)



hasPaddleCollidedWithBoundary :: Paddle -> Float -> Bool
hasPaddleCollidedWithBoundary paddle boundaryPosY =
  let
    distanceY = (getY $ _paddle_pos paddle) + boundaryPosY
  in
    distanceY <= (_paddle_height paddle / 2)


applyCollision :: Ball -> Ball -> Ball
applyCollision ball1@(Ball pos size _ label color) ball2 =
  let newVelocity = calculateBounceVelocity ball1 ball2
      newPosition = pos + calculateOverlap ball1 ball2
  in Ball newPosition size newVelocity label color



buildScene :: Renderer os -> GameState -> GLFWContext os (Scene os)
buildScene renderer (GameState player1 player2 ball gizmoA gizmoB) = do
  let players = [player1, player2]
  let actors = [Actor $ ball]--, Actor $ gizmoA, Actor $ gizmoB]
  playerRenderables <- mapM (createRenderable renderer) players
  actorRenderables <- mapM (\(Actor a) -> createRenderable renderer a) actors
  pure $ Scene (playerRenderables <> actorRenderables)



type KeyState = Int
data InputKeyMap = InputKeyMap
  { _inputKey_w :: KeyState
  , _inputKey_s :: KeyState
  , _inputKey_up :: KeyState
  , _inputKey_down :: KeyState
  }


newtype TimeDelta = TimeDelta Float

mapKeyState :: Maybe GLFW.KeyState -> KeyState
mapKeyState = \case
  Just GLFW.KeyState'Pressed -> 1
  Just GLFW.KeyState'Released -> 0
  Just GLFW.KeyState'Repeating -> 0
  Nothing -> 0


instance RespondsToInput Player where
  move (TimeDelta timeDelta) player =
    let playerInput = _player_input player
        upwardMovement = (fromIntegral $ _playerInput_up playerInput) * timeDelta * 1
        downwardMovement = (fromIntegral $ _playerInput_down playerInput) * timeDelta * (-1)
        movementDelta = upwardMovement + downwardMovement
        paddle = _player_paddle player
        (V2 paddleX paddleY) = _paddle_pos $ paddle

    in player
       { _player_paddle = paddle
                          { _paddle_pos = V2 paddleX (paddleY + movementDelta)
                          }
       }



moveBall :: Ball -> TimeDelta -> Ball
moveBall (Ball pos size (V2 velX velY) label color) (TimeDelta timeDelta) =
  let movementVector = V2 (velX * timeDelta) (velY * timeDelta)
  in Ball (pos + movementVector) size (V2 velX velY) label color


handleCollision :: GameState -> Ball
handleCollision gameState =
  let
    ball@(Ball bPos bSize bVel _ _) = _gameState_ball gameState
    playerPaddles = [ (_player_paddle . _gameState_player1) gameState
                    , (_player_paddle . _gameState_player2) gameState
                    ]

    mCollided = find (hasCollided ball) playerPaddles
  in
    case mCollided of
      Nothing -> handleCollisionWithBoundary ball
      Just paddle@(Paddle pPos pWidth pHeight _) ->
        let
          circleColliderX = ((getX pPos) + (pWidth / 2))
          circleColliderY = clamp ((getY pPos) - (pHeight / 2), (getY pPos) + (pHeight / 2)) $ getY bPos
          collisionPoint = V2 circleColliderX circleColliderY
        in
          applyCollision ball (Ball collisionPoint pWidth (negate <$> bVel) "paddle-ball" (V4 0 0 0 0))



handleCollisionWithBoundary :: Ball -> Ball
handleCollisionWithBoundary ball =
  let
    collidedTop = hasBallCollidedWithBoundary ball 1
    
    collidedBottom = hasBallCollidedWithBoundary ball (-1)
  in
    if collidedTop then
      let
        colliderY = 1 + (_ball_size ball / 2)
        colliderY' = DT.trace ("Collided top") colliderY
        collisionPoint = V2 (getX $ _ball_pos ball) colliderY'
      in
        applyCollision ball (Ball collisionPoint (_ball_size ball) (negate <$> (_ball_velocity ball)) "boundary-ball" (V4 0 0 0 0))
    else if collidedBottom then
      let
        colliderY = (-1) - (_ball_size ball / 2)
        colliderY' = DT.trace ("Collided bottom") colliderY
        collisionPoint = V2 (getX $ _ball_pos ball) colliderY'
      in
        applyCollision ball (Ball collisionPoint (_ball_size ball) (negate <$> (_ball_velocity ball)) "boundary-ball" (V4 0 0 0 0))
    else
      ball

getX :: V2 a -> a
getX (V2 x _) = x

getY :: V2 a -> a
getY (V2 _ y) = y

absV2 :: V2 Float -> V2 Float
absV2 (V2 x y) = V2 (abs x) (abs y)

add :: V2 Float -> V2 Float -> V2 Float
add (V2 x1 y1) (V2 x2 y2) = V2 (x1 + x2) (y1 + y2)

sub :: V2 Float -> V2 Float -> V2 Float
sub (V2 x1 y1) (V2 x2 y2) = V2 (x1 - x2) (y1 - y2)

lengthV2 :: V2 Float -> Float
lengthV2 (V2 x y) = sqrt (x*x + y*y)

perpendicularV2 :: V2 Float -> V2 Float
perpendicularV2 (V2 x y) = V2 y (-x)

normalizeV2 :: V2 Float -> V2 Float
normalizeV2 v =
  let magnitude = sqrt (((getX v) ** 2) + (getY v) ** 2)
  in V2 (getX v / magnitude) (getY v / magnitude)

scalarMultiply :: V2 Float -> Float -> V2 Float
scalarMultiply (V2 vx vy) s = V2 (vx * s) (vy * s)