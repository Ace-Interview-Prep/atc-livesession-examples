module BankAccount where

import Control.Monad.State
import Control.Monad.Identity

-- Account state
data Account = Account { balance :: Int }
  deriving (Show)

-- State monad for account operations
type AccountM a = StateT Account Identity a

-- Deposit an amount
deposit :: Int -> AccountM Bool
deposit amount
  | amount > 0 = do
      modify (\acc -> acc { balance = balance acc + amount })
      return True
  | otherwise = return False

-- Withdraw an amount
withdraw :: Int -> AccountM Bool
withdraw amount = do
  acc <- get
  if amount > 0 && balance acc >= amount
    then do
      modify (\acc -> acc { balance = balance acc - amount })
      return True
    else return False

-- Simulate a transfer to another account
transfer :: Int -> AccountM Account -> AccountM Bool
transfer amount other = do
  success <- withdraw amount
  if success
    then do
      otherAccount <- other  -- No `lift`, as `other` is already `AccountM Account`
      let newBalance = balance otherAccount + amount
      return True  -- Simulate success; in a real system, update other account
    else return False

-- Example: Get another account (mocked for simplicity)
getOtherAccount :: AccountM Account
getOtherAccount = return (Account 100)

-- Run an AccountM computation
runAccount :: AccountM a -> Account -> (a, Account)
runAccount m initial = runIdentity (runStateT m initial)

-- Main function to test
main :: IO ()
main = do
  let initialAccount = Account 200
      commands = [deposit 50, withdraw 100, transfer 50 getOtherAccount]
  putStrLn "Running commands on initial account: "
  print initialAccount
  let results = foldl (\(res, acc) m -> let (r, acc') = runAccount m acc in (r : res, acc'))
                      ([], initialAccount) commands
  putStrLn "Results:"
  mapM_ (\(i, r) -> putStrLn $ "  Command " ++ show i ++ ": " ++ show r) (zip [1..] (reverse $ fst results))
  putStrLn "Final Account:"
  print (snd results)
