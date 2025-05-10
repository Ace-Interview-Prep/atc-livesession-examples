
module Main where

import Control.Monad.State



-- run = Int -> Maybe (Int, Int)
incrementT :: StateT Int Maybe Int
incrementT = do
  s <- get
  if s < 10
    then do
      put (s + 1)
      pure s
    else lift Nothing




main :: IO ()
main = do
  putStrLn "Running incrementT with initial state = 5:"
  let result = runStateT incrementT 5
  case result of
    Just (t, newState) -> putStrLn $ "Result: " ++ show t ++ ", New State: " ++ show newState
    Nothing -> putStrLn "Computation failed"
  putStrLn "Running incrementT with initial state = 10:"
  let result2 = runStateT incrementT 10
  case result2 of
    Just (t, newState) -> putStrLn $ "Result: " ++ show t ++ ", New State: " ++ show newState
    Nothing -> putStrLn "Computation failed"








-- class MonadTrans t where
--   lift :: Monad m => m a -> t m a

-- newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

-- instance (Monad m) => Monad (StateT s m) where
--   return x = StateT $ \s -> return (x, s)
--   m >>= k = StateT $ \s -> do
--     (a, s') <- runStateT m s
--     runStateT (k a) s'
