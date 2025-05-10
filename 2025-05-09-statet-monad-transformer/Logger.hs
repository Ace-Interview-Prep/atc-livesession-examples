-- {-# LANGUAGE UndecidableInstances #-}

-- module LoggerT where

-- import Control.Monad.State
-- import Control.Monad.IO.Class
-- import Control.Applicative (liftA2)

-- -- LoggerT Transformer
-- newtype LoggerT w m a = LoggerT { runLoggerT :: m (a, w) }

-- -- Functor instance
-- instance (Functor m) => Functor (LoggerT w m) where
--   fmap f (LoggerT m) = LoggerT $ fmap (\(a, w) -> (f a, w)) m

-- -- Applicative instance
-- instance (Monoid w, Monad m) => Applicative (LoggerT w m) where
--   pure x = LoggerT $ return (x, mempty)
--   LoggerT mf <*> LoggerT mx = LoggerT $ do
--     (f, w1) <- mf
--     (x, w2) <- mx
--     return (f x, w1 <> w2)

-- -- Monad instance
-- instance (Monoid w, Monad m) => Monad (LoggerT w m) where
--   return = pure
--   LoggerT m >>= k = LoggerT $ do
--     (a, w1) <- m
--     let LoggerT m' = k a
--     (b, w2) <- m'
--     return (b, w1 <> w2)

-- -- MonadTrans instance
-- instance (Monoid w) => MonadTrans (LoggerT w) where
--   lift m = LoggerT $ do
--     a <- m
--     return (a, mempty)

-- -- MonadIO instance
-- instance (Monoid w, MonadIO m) => MonadIO (LoggerT w m) where
--   liftIO = lift . liftIO

-- -- MonadState instance
-- instance (Monoid w, MonadState s m) => MonadState s (LoggerT w m) where
--   get = lift get
--   put = lift . put
--   state f = LoggerT $ do
--     a <- state f
--     return (a, mempty)

-- -- Log a message
-- logMessage :: (Monoid w, Monad m) => w -> LoggerT w m ()
-- logMessage msg = LoggerT $ return ((), msg)

-- -- Example: Web server with LoggerT and StateT
-- type UserId = String
-- type Session = [(UserId, Int)]  -- Tracks user request counts
-- type WebM a = LoggerT [String] (StateT Session IO) a

-- -- Process a user request
-- processRequest :: UserId -> WebM String
-- processRequest userId = do
--   session <- get
--   let count = lookup userId session
--       newCount = maybe 1 (+1) count
--   put $ (userId, newCount) : filter ((/= userId) . fst) session
--   logMessage ["Processed request for " ++ userId ++ ", count: " ++ show newCount]
--   liftIO $ putStrLn $ "Handling request for " ++ userId
--   return $ "Response for " ++ userId ++ ": " ++ show newCount ++ " requests"

-- -- Run the web computation
-- runWebM :: WebM a -> Session -> IO (a, Session, [String])
-- runWebM m initial = do
--   ((a, logs), finalSession) <- runStateT (runLoggerT m) initial
--   return (a, finalSession, logs)

-- -- Main function to test
-- main :: IO ()
-- main = do
--   let initialSession = []
--       requests = [processRequest "alice", processRequest "bob", processRequest "alice"]
--   putStrLn "Simulating web requests:"
--   (results, finalSession, logs) <- runWebM (sequence requests) initialSession
--   putStrLn "Responses:"
--   mapM_ (\r -> putStrLn $ "  " ++ r) results
--   putStrLn "Final Session:"
--   print finalSession
--   putStrLn "Logs:"
--   mapM_ (\l -> putStrLn $ "  " ++ l) logs







{-# LANGUAGE GeneralizedNewtypeDeriving, DerivingStrategies #-}

module LoggerT where

import Control.Monad.State
import Control.Monad.IO.Class

-- LoggerT Transformer
newtype LoggerT w m a = LoggerT { runLoggerT :: m (a, w) }
  deriving newtype (Functor, Applicative, Monad, MonadIO)

-- MonadTrans instance
instance (Monoid w) => MonadTrans (LoggerT w) where
  lift m = LoggerT $ do
    a <- m
    return (a, mempty)

-- Log a message
logMessage :: (Monoid w, Monad m) => w -> LoggerT w m ()
logMessage msg = LoggerT $ return ((), msg)

-- Example: Web server with LoggerT and StateT
type UserId = String
type Session = [(UserId, Int)]  -- Tracks user request counts
--type WebM a = LoggerT [String] (StateT Session IO) a

-- MonadState instance for WebM
newtype WebM a = WebM (LoggerT [String] (StateT Session IO) a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadState Session)

-- Process a user request
processRequest :: UserId -> WebM String
processRequest userId = WebM $ do
  session <- get
  let count = lookup userId session
      newCount = maybe 1 (+1) count
  put $ (userId, newCount) : filter ((/= userId) . fst) session
  logMessage ["Processed request for " ++ userId ++ ", count: " ++ show newCount]
  liftIO $ putStrLn $ "Handling request for " ++ userId
  return $ "Response for " ++ userId ++ ": " ++ show newCount ++ " requests"

-- Run the web computation
runWebM :: WebM a -> Session -> IO (a, Session, [String])
runWebM (WebM m) initial = do
  ((a, logs), finalSession) <- runStateT (runLoggerT m) initial
  return (a, finalSession, logs)

-- Main function to test
main :: IO ()
main = do
  let initialSession = []
      requests = [processRequest "alice", processRequest "bob", processRequest "alice"]
  putStrLn "Simulating web requests:"
  (results, finalSession, logs) <- runWebM (sequence requests) initialSession
  putStrLn "Responses:"
  mapM_ (\r -> putStrLn $ "  " ++ r) results
  putStrLn "Final Session:"
  print finalSession
  putStrLn "Logs:"
  mapM_ (\l -> putStrLn $ "  " ++ l) logs
