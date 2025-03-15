module Main where

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Data.List as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as LBS

import Control.Monad.Trans.Reader
import Text.Parsec
import Scrappy.Scrape
import Control.Monad.Trans.Class

url :: String 
url = "https://acetalent.io/landing/base"

main :: IO ()
main = do
  let
    req :: Maybe Request 
    req = parseRequest url  
  case req of
    Nothing -> print "end program"
    Just req -> do
      mgr1 :: Manager <- newManager tlsManagerSettings --defaultManagerSettings 
      res  <- httpLbs req mgr1
      let body :: LBS.ByteString = responseBody res
      let bodyString :: String = T.unpack $ T.decodeUtf8 $ LBS.toStrict body
      print $ scrape (string "Ace") bodyString


      print "some log"

      -- writeLog "some log" :: WriterT [Log] m ()
      -- when failed $ do
      --   logs <- getLogs
      --   prettyprint logs
      
      pure () 
  pure () 

type Log = String 


safeDiv :: Int -> Int -> Either String Int
safeDiv numerator denom = case denom of
  0 -> Left "Tried to divide by zero"
  _ -> Right $ div numerator denom






main' = do
  mgr1 :: Manager <- newManager tlsManagerSettings
  
  runReaderT someAction mgr1 

someAction :: (ReaderT Manager) IO () 
someAction = do 

  mgrFromConfig <- ask

  res <- lift $ httpLbs (undefined) mgrFromConfig
  
  pure () 


-- val :: Maybe Int 
-- val = Just 1

-- val2 :: Maybe Int 
-- val2 = Nothing


-- val2 :: Either DBError String
-- val2 = Left NotExist

-- -- = Right "myUsername" 

-- case val2 of
--   Left NotExist -> writeResponse "the requested username does not exist"
--   Right usern -> writeResponse usern

-- bimap :: 

-- example :: Maybe String
-- example = do
--   myInt <- val
--   my2Int <- val2
--   case val2 of
--     Nothing -> Nothing
--     Just v -> Just ( v + 1)  
--   pure $ show myInt ++ "String"

-- -- example == "1String"
-- -- example == Nothing
