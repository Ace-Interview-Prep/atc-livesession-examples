
module Adventure2 where

import Control.Monad (unless, when)
import Control.Monad.State
import Control.Monad.IO.Class
import Data.List (find)
import Data.Maybe (isJust)

-- ErrorT Transformer
newtype ErrorT e m a = ErrorT { runErrorT :: m (Either e a) }

-- Functor instance
instance (Functor m) => Functor (ErrorT e m) where
  fmap f (ErrorT m) = ErrorT $ fmap (fmap f) m

-- Applicative instance
instance (Monad m) => Applicative (ErrorT e m) where
  pure x = ErrorT $ return (Right x)
  ErrorT mf <*> ErrorT mx = ErrorT $ do
    f <- mf
    x <- mx
    return (f <*> x)

-- Monad instance
instance (Monad m) => Monad (ErrorT e m) where
  return = pure
  ErrorT m >>= k = ErrorT $ do
    result <- m
    case result of
      Left e -> return (Left e)
      Right a -> runErrorT (k a)

-- MonadTrans instance
instance MonadTrans (ErrorT e) where
  lift m = ErrorT $ do
    a <- m
    return (Right a)

-- MonadIO instance
instance (MonadIO m) => MonadIO (ErrorT e m) where
  liftIO = lift . liftIO

-- MonadState instance for AdventureM
instance MonadState GameState (ErrorT String (StateT GameState IO)) where
  get = ErrorT $ do
    s <- get
    return (Right s)
  put s = ErrorT $ do
    put s
    return (Right ())
  state f = ErrorT $ do
    a <- state f
    return (Right a)

-- Error handling operations
throwError :: (Monad m) => e -> ErrorT e m a
throwError e = ErrorT $ return (Left e)

catchError :: (Monad m) => ErrorT e m a -> (e -> ErrorT e m a) -> ErrorT e m a
catchError (ErrorT m) handler = ErrorT $ do
  result <- m
  case result of
    Left e -> runErrorT (handler e)
    Right a -> return (Right a)

-- Game state
type Position = (Int, Int)
type Item = String
data GameState = GameState
  { playerPos :: Position
  , inventory :: [Item]
  , rooms :: [(Position, Maybe Item, Bool)]  -- (position, item, isTrap)
  } deriving (Show)

-- Adventure monad stack
type AdventureM a = ErrorT String (StateT GameState IO) a

-- Initial game state
initialState :: GameState
initialState = GameState
  { playerPos = (0, 0)
  , inventory = []
  , rooms = [ ((0, 0), Nothing, False)
            , ((0, 1), Just "key", False)
            , ((1, 0), Just "map", True)
            , ((1, 1), Nothing, False) ]  -- Exit
  }

-- Game actions
move :: String -> AdventureM ()
move dir = do
  state <- get
  let (x, y) = playerPos state
      newPos = case dir of
        "north" -> (x, y + 1)
        "south" -> (x, y - 1)
        "east"  -> (x + 1, y)
        "west"  -> (x - 1, y)
        _       -> (x, y)
  room <- lookupRoom newPos
  case room of
    Nothing -> throwError "Invalid move: out of bounds"
    Just (_, _, True) -> throwError "You hit a trap! Game over."
    Just _ -> put state { playerPos = newPos }

takeItem :: AdventureM ()
takeItem = do
  state <- get
  let pos = playerPos state
  room <- lookupRoom pos
  case room of
    Nothing -> throwError "No room here"
    Just (p, Nothing, _) -> throwError "No item to take"
    Just (p, Just item, _) -> do
      put state { inventory = item : inventory state
                , rooms = (p, Nothing, False) : filter ((/= p) . (\(p',_,_) -> p')) (rooms state) }
      liftIO $ putStrLn $ "Picked up " ++ item

lookupRoom :: Position -> AdventureM (Maybe (Position, Maybe Item, Bool))
lookupRoom pos = do
  state <- get
  return $ find (\(p, _, _) -> p == pos) (rooms state)

checkWin :: AdventureM Bool
checkWin = do
  state <- get
  return $ playerPos state == (1, 1) && "key" `elem` inventory state

-- Main game loop
gameLoop :: AdventureM ()
gameLoop = do
  state <- get
  liftIO $ do
    putStrLn $ "\nYou are at " ++ show (playerPos state)
    putStrLn $ "Inventory: " ++ show (inventory state)
    putStrLn "Available actions: move <north/south/east/west>, take, quit"
    putStr "Action: "
  action <- liftIO getLine
  case words action of
    ["move", dir] -> move dir `catchError` \e -> liftIO (putStrLn e)
    ["take"] -> takeItem `catchError` \e -> liftIO (putStrLn e)
    ["quit"] -> liftIO $ putStrLn "Goodbye!"
    _ -> liftIO $ putStrLn "Invalid action"
  won <- checkWin
  unless won gameLoop
  when won $ liftIO $ putStrLn "You won! Found the exit with the key!"

-- Run the game
runAdventureM :: AdventureM a -> GameState -> IO (Either String a, GameState)
runAdventureM m initial = runStateT (runErrorT m) initial

-- Main function
main :: IO ()
main = do
  putStrLn "Welcome to the Maze Adventure!"
  (result, finalState) <- runAdventureM gameLoop initialState
  case result of
    Left err -> putStrLn $ "Game ended with error: " ++ err
    Right _ -> putStrLn "Game completed"
  print finalState
