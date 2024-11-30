module Main where

--import Network.Wai
import Network.Wai.Handler.Warp

import API (app)
import Database (migrateDb)

main :: IO ()
main = do
  migrateDb
  run 8081 app
  putStrLn "Server started on port 8081!"
