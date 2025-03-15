{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DistributedMonteCarlo where

import Control.Concurrent (forkIO, newChan, readChan, writeChan, threadDelay)
import Control.Concurrent.MVar (newMVar, takeMVar, putMVar, MVar)
import Control.Monad (forM_, replicateM, liftM2)
import Network.HTTP.Simple (httpJSON, getResponseBody, parseRequest, setRequestBodyLBS, setRequestHeader)
import Data.Aeson (FromJSON, ToJSON, object, (.=), decode, encode)
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Char8 as BS
import System.Random (randomRIO)
import GHC.Generics (Generic)
import Network.Wai (Application, Request, requestBody, responseLBS, strictRequestBody, rawQueryString)
import Network.HTTP.Types (status200, status400, parseQuery)
import System.IO (hFlush, stdout)
import Control.Exception (SomeException, catch)



data NodeConfig = NodeConfig
  { _nodeConfig_nodeId :: Int
  , _nodeConfig_nodeUrl :: String
  } deriving (Show)

data ComputeRequest = ComputeRequest
  { _computeRequest_points :: Int
  } deriving (Show, Generic)

data ComputationResult = ComputationResult
  { _computationResult_pointsInside :: Int
  , _computationResult_totalPoints :: Int
  } deriving (Show, Generic)

data PiResponse = PiResponse
  { _piResponse_calculatedPi :: Double
  } deriving (Show, Generic)

instance ToJSON ComputeRequest
instance FromJSON ComputeRequest
instance ToJSON ComputationResult
instance FromJSON ComputationResult
instance ToJSON PiResponse


monteCarloPi :: Int -> IO ComputationResult
monteCarloPi numPoints = do
  inside <- countPointsInside numPoints
  return $ ComputationResult inside numPoints
  where
    countPointsInside :: Int -> IO Int
    countPointsInside n = length . (filter insideCircle) <$> replicateM n randomPoint

    randomPoint :: IO (Double, Double)
    randomPoint = liftM2 (,) (randomRIO (-1,1)) (randomRIO (-1,1))

    insideCircle :: (Double, Double) -> Bool
    insideCircle (x, y) = x*x + y*y <= 1


distributeMonteCarlo :: Int -> Int -> IO Double
distributeMonteCarlo numNodes totalPoints = do
  let pointsPerNode = totalPoints `div` numNodes
  let baseUrl = "http://worker-service:8080/compute"
  let nodes = [NodeConfig i baseUrl | i <- [1..numNodes]]

  resultChan <- newChan
  resultVar <- newMVar (0, 0)

  forM_ nodes $ \node -> forkIO $ do
    result <- retryComputeOnNode node pointsPerNode 5
    writeChan resultChan result

  result <- replicateM numNodes $ readChan resultChan
  final <- takeMVar resultVar
  let (totalInside, totalCount) = foldl (\(i, t) r ->
                                           ( i + (_computationResult_pointsInside r)
                                           , t + (_computationResult_totalPoints r)
                                           )
                                        ) final result

  putMVar resultVar (totalInside, totalCount)
  return $ 4.0 * fromIntegral totalInside / fromIntegral totalCount


retryComputeOnNode :: NodeConfig -> Int -> Int -> IO ComputationResult
retryComputeOnNode node pointsToCompute retries = go retries
  where
    go 0 = do
        putStrLn $ "Failed to contact node " ++ show (_nodeConfig_nodeId node) ++ " after retries"
        hFlush stdout
        return $ ComputationResult 0 pointsToCompute
    go n = do
        result <- computeOnNode node pointsToCompute `catch` \(e :: SomeException) -> do
            putStrLn $ "Attempt " ++ show (retries - n + 1) ++ " failed for node " ++ show (_nodeConfig_nodeId node) ++ ": " ++ show e
            hFlush stdout
            threadDelay 1000000
            go (n - 1)
        return result


computeOnNode :: NodeConfig -> Int -> IO ComputationResult
computeOnNode node pointsToCompute = do
  let requestBody = encode $ ComputeRequest pointsToCompute
  req <- parseRequest $ "POST " ++ _nodeConfig_nodeUrl node
  let reqWithBody = setRequestBodyLBS requestBody req
  let reqWithHeaders = setRequestHeader "Content-Type" ["application/json"] reqWithBody
  resp <- httpJSON reqWithHeaders
  return $ getResponseBody resp


masterApp :: Application
masterApp req respond = do
  putStrLn "Received request at master"
  hFlush stdout
  let query = parseQuery $ rawQueryString req
  case lookup "points" query of
    Just (Just pointsStr) -> do
      let pointsNum = read (BS.unpack pointsStr) :: Int
      putStrLn $ "Computing Pi with " ++ show pointsNum ++ " points"
      hFlush stdout
      piValue <- distributeMonteCarlo 10 pointsNum
      putStrLn $ "Calculated Pi: " ++ show piValue
      respond $ responseLBS status200
        [("Content-Type", "application/json")]
        (encode $ PiResponse piValue)
    Nothing -> respond $ responseLBS status400 [] "Missing or invalid 'points' parameter"


workerApp :: Application
workerApp req respond = do
  putStrLn "Received request at worker"
  hFlush stdout
  body <- strictRequestBody req
  case decode body :: Maybe ComputeRequest of
    Just computeReq -> do
      putStrLn $ "Points: " ++ (show $ _computeRequest_points computeReq)
      hFlush stdout
      result <- monteCarloPi (_computeRequest_points computeReq)
      putStrLn $ "Response: " ++ (show result)
      hFlush stdout
      respond $ responseLBS status200
        [("Content-Type", "application/json")]
        (encode result)
    Nothing -> respond $ responseLBS status400 [] "Invalid request"
