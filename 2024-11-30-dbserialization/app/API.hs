{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module API (app) where

import Prelude hiding (product)
import Data.Text
import qualified Data.Text as T

{- import Data.List hiding (product) -}
--import Data.Time.Calendar
import Network.Wai
--import Network.Wai.Handler.Warp
import Servant

import Database
import Product
import Shop

{- type ConsumerID = String
type MachineID = String
type StoreID = String -}

type BackendRoutes = "api" :> "product" :> "create" :> ReqBody '[JSON] Product :> Post '[JSON] Text
  :<|> "api" :> "shop" :> "create" :> ReqBody '[JSON] Shop :> Post '[JSON] Text

type API = BackendRoutes

server :: Server API
server = createProduct
  :<|> createShop

  where createProduct :: Product -> Handler Text
        createProduct product = runDb $ do
          res <- insertProduct product
          pure $ case res of
            (Just e) -> T.pack $ show e
            Nothing -> T.pack $ "Success"

        createShop :: Shop -> Handler Text
        createShop shop = runDb $ do
          res <- insertShop shop
          pure $ case res of
            (Just e) -> T.pack $ show e
            Nothing -> T.pack $ "Success"

proxy :: Proxy API
proxy = Proxy

app :: Application
app = serve proxy server     