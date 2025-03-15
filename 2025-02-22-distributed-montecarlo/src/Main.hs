module Main where

import DistributedMonteCarlo
import System.IO (hFlush, stdout)
import System.Environment (getArgs)
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["master", pointStr] -> do
      putStrLn "Starting master node on port 8080"
      hFlush stdout
      run 8080 masterApp
    ["worker"] -> do
      putStrLn "Starting worker node on port 8080"
      hFlush stdout
      run 8080 workerApp
    _ -> putStrLn "Usage: master <points> | worker"
