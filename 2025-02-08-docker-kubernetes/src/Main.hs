{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Monad (forever)
import Data.Matrix (Matrix, fromList, multStd)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as B
import Network.Wai (pathInfo, responseLBS, getRequestBodyChunk)
import Network.Wai.Handler.Warp
import Network.HTTP.Simple
import Network.HTTP.Types

type SharedMatrix = TVar (Maybe (Matrix Int))

worker :: SharedMatrix -> SharedMatrix -> IO ()
worker input output = forever $ do
  maybeMatrix <- atomically $ do
    matrix <- readTVar input
    writeTVar input Nothing
    return matrix
  case maybeMatrix of
    Just matrix -> do
      let result = matrix `multStd` matrix
      atomically $ writeTVar output (Just result)
      putStrLn "Computation completed and result stored."
    Nothing -> threadDelay 100000

main :: IO ()
main = do
  inputVar <- newTVarIO Nothing
  outputVar <- newTVarIO Nothing

  forkIO $ worker inputVar outputVar
  forkIO $ worker inputVar outputVar

  putStrLn "Service running..."
  httpServer inputVar outputVar

httpServer :: SharedMatrix -> SharedMatrix -> IO ()
httpServer inputVar outputVar = do
  _ <- forkIO $ runServer inputVar outputVar
  forever $ threadDelay 1000000

runServer :: SharedMatrix -> SharedMatrix -> IO ()
runServer inputVar outputVar = do
  putStrLn "Starting HTTP server on port 8080..."
  _ <- run 8080 $ \req respond -> do
    case pathInfo req of
      ["compute"] -> do
        body <- getRequestBodyChunk req
        let matrix = fromList 2 2 (map read $ words $ B.unpack body) :: Matrix Int
        atomically $ writeTVar inputVar (Just matrix)
        result <- atomically $ do
          maybeResult <- readTVar outputVar
          case maybeResult of
            Just r -> do
              writeTVar outputVar Nothing
              return r
            Nothing -> retry
        respond $ responseLBS status200 [] (BL.pack $ show result)
      _ -> respond $ responseLBS status404 [] (BL.pack "Not found")
  return ()
