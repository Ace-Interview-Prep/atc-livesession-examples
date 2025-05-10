
module Bank where

import Control.Monad.State
import Control.Monad.Reader

data Account = Account { balance :: Int } deriving Show
type Bank = [Account]
data Config = Config { minBalance :: Int } deriving Show
type BankM a = StateT Bank (ReaderT Config IO) a

withdraw :: Int -> BankM Bool
withdraw amount = do
  [acc] <- get
  cfg <- ask
  if amount > 0 && balance acc - amount >= minBalance cfg
    then do
      put [Account (balance acc - amount)]
      return True
    else return False

runBankM :: BankM a -> Bank -> Config -> IO (a, Bank)
runBankM m bank cfg = runReaderT (runStateT m bank) cfg

main :: IO ()
main = do
  let bank = [Account 100]
      cfg = Config 50
  (result, finalBank) <- runBankM (withdraw 30) bank cfg
  putStrLn $ "Withdrawal: " ++ show result
  putStrLn $ "Final Bank: " ++ show finalBank
