{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Database where

import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.Trans.Reader

import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.TH

import qualified Data.Text as T

import TypeClasses
import Shop (Shop(..), ShopNumber, mkShop)
import Product (Product(..), ProductError(ShopDoesNotExist), SerialNumber, mkProduct)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
ShopRegistry
    shopNumber String
    shopLocationCode String
    shopName String
    UShopRegistry shopNumber shopLocationCode
    deriving Show
ProductRegistry
    serialNumber String
    shopId ShopRegistryId
    productName String    
    UProductRegistry serialNumber shopId
    deriving Show
|]

connectionString :: ConnectionString
connectionString = "host=localhost dbname=dbserialization user=rhemsuda password=password123 port=5432"

runDb :: (MonadIO m) => SqlPersistT IO a -> m a
runDb action = liftIO $ runStderrLoggingT $
  withPostgresqlConn connectionString $ \backend ->
  liftIO $ runReaderT action backend

migrateDb :: IO ()
migrateDb = runDb $ runMigration migrateAll

data InsertError
  = RecordAlreadyStored String
  | RecordKeyNotFound String

instance Show InsertError where
  show (RecordAlreadyStored s) = s ++ " has already been stored."
  show (RecordKeyNotFound s) = "Couldn't find record in database: " ++ s

{- insertProduct :: Product -> SqlPersistT IO (Maybe InsertError)
insertProduct (Product serialNum productName) = do
  res <- insertUnique $ ProductRegistry 
         (T.unpack $ getText serialNum)
         (T.unpack $ getText productName)
  pure $ case res of
    Just _ -> Nothing
    Nothing -> Just $ RecordAlreadyStored $ show serialNum -}

-- Function to insert a ProductRegistry if the shopNumber exists
insertProduct :: Product -> SqlPersistT IO (Maybe InsertError)
insertProduct (Product shop serialNumber productName) = do
  maybeShop <- getBy $ UShopRegistry 
               (T.unpack $ getText $ shopNumber shop) 
               (T.unpack $ getText $ shopLocationCode shop)
  case maybeShop of
    Nothing -> pure $ Just (RecordKeyNotFound $ show (ShopDoesNotExist $ T.unpack $ getText $ shopNumber shop))
    Just (Entity shopId _) -> do
      res <- insertUnique $ ProductRegistry
             (T.unpack $ getText serialNumber)
             shopId
             (T.unpack $ getText productName)
      pure $ case res of
        Just _ -> Nothing
        Nothing -> Just $ RecordAlreadyStored $ show serialNumber


insertShop :: Shop -> SqlPersistT IO (Maybe InsertError)
insertShop (Shop num loc name) = do
  res <- insertUnique $ ShopRegistry
         (T.unpack $ getText num)
         (T.unpack $ getText loc)
         (T.unpack $ getText name)
  pure $ case res of
    Just _ -> Nothing
    Nothing -> Just $ RecordAlreadyStored $ show loc ++ " " ++ show num

{- data RegistrationError
  = InvalidStoreNumber ShopNumber
  | ShopNumberNotFound ShopNumber
  | AlreadyRegistered SerialNumber -}
