module StackCalculator where

import Control.Monad.State
import Data.Maybe (fromMaybe)
import Data.Char (isDigit)

-- Stack as a list
type Stack = [Int]

-- Explicit state passing
pushExplicit :: Int -> Stack -> (Maybe Int, Stack)
pushExplicit x stack = (Just x, x : stack)

popExplicit :: Stack -> (Maybe Int, Stack)
popExplicit [] = (Nothing, [])
popExplicit (x:xs) = (Just x, xs)

evalExplicit :: String -> Stack -> (Maybe Int, Stack)
evalExplicit "push 5" s = pushExplicit 5 s
evalExplicit "pop" s = popExplicit s
evalExplicit _ s = (Nothing, s)

-- State monad
type StackState a = State Stack a

push :: Int -> StackState ()
push x = modify (x:)

pop :: StackState (Maybe Int)
pop = do
  stack <- get
  case stack of
    [] -> return Nothing
    (x:xs) -> do
      put xs
      return (Just x)

-- Parse and evaluate commands
eval :: String -> StackState (Maybe Int)
eval cmd
  | "push " `isPrefixOf` cmd = do
      let numStr = drop 5 cmd
      if all isDigit numStr
        then do
          let n = read numStr :: Int
          push n
          return (Just n)
        else return Nothing
  | cmd == "pop" = pop
  | otherwise = return Nothing
  where
    isPrefixOf prefix str = take (length prefix) str == prefix

-- Run evaluation
runEval :: String -> Stack -> (Maybe Int, Stack)
runEval cmd stack = runState (eval cmd) stack

-- Main function to demonstrate runEval
main :: IO ()
main = do
  let commands = ["push 5", "push 10", "pop", "pop", "pop"]
      initialStack = []
      -- Process commands sequentially, threading the stack
      results = foldl (\(res, stk) cmd -> let (r, s') = runEval cmd stk in (r : res, s'))
                      ([], initialStack) commands
  putStrLn "Results (reversed order):"
  mapM_ (\r -> putStrLn $ "  " ++ show r) (reverse $ fst results)
  putStrLn $ "Final Stack: " ++ show (snd results)
