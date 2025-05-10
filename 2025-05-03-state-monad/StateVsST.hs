module StateVsST where

import Control.Monad.State
import Control.Monad.ST
import Data.STRef

-- State monad example: Increment a counter three times
exampleState :: State Int Int
exampleState = do
  modify (+1)  -- Increment counter
  modify (+1)
  modify (+1)
  get          -- Return final counter value

-- Run State example
runExampleState :: Int -> (Int, Int)
runExampleState initial = runState exampleState initial

-- ST monad example: Increment a counter three times using STRef
exampleST :: ST s Int
exampleST = do
  counter <- newSTRef 0  -- Initialize mutable counter
  modifySTRef counter (+1)  -- Increment
  modifySTRef counter (+1)
  modifySTRef counter (+1)
  readSTRef counter         -- Return final value

-- Run ST example
runExampleST :: Int
runExampleST = runST exampleST

-- Main function to run both examples
main :: IO ()
main = do
  putStrLn "Running State monad example with initial counter = 0:"
  let (stateResult, finalState) = runExampleState 0
  putStrLn $ "  Result: " ++ show stateResult
  putStrLn $ "  Final State: " ++ show finalState
  putStrLn "Running ST monad example:"
  let stResult = runExampleST
  putStrLn $ "  Result: " ++ show stResult
