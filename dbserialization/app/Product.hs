{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

--module Product (Product, SerialNumber, mkProduct) where
module Product where

import Data.Text
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Aeson.Parser
import Data.Aeson.Key (fromString)
import qualified Data.Text as T
import GHC.Generics (Generic)
import TypeClasses
import Shop

data ProductError
  = InvalidSerialNumber
  | ProductNameTooLong
  | ShopError ShopError
  | ShopDoesNotExist String

instance Show ProductError where
  show InvalidSerialNumber = "The serial number you provided is invalid."
  show ProductNameTooLong = "The product name is too long."
  show (ShopDoesNotExist s) = "The shop with ID " ++ (show s) ++ " does not exist."

newtype SerialNumber = SerialNumber Text
  deriving (ToJSON, FromJSON, Generic, Eq, Show)
instance IsText SerialNumber where
  getText (SerialNumber sn) = sn

newtype ProductName = ProductName Text
  deriving (ToJSON, FromJSON, Generic, Eq, Show)
instance IsText ProductName where
  getText (ProductName pn) = pn

data Product = Product 
  { shop :: Shop
  , serialNumber :: SerialNumber 
  , productName :: ProductName
  } deriving Generic

instance ToJSON Product
instance FromJSON Product where
  parseJSON = withObject "Product" $ \o -> do
    res <- mkProduct
      <$> o .: fromString "shop"
      <*> o .: fromString "serialNumber"
      <*> o .: fromString "productName"
    case res of
      Right product -> pure product
      Left e -> fail $ show e

mkSerialNumber :: Text -> Either ProductError SerialNumber
mkSerialNumber s
  | T.length s /= 8 = Left $ InvalidSerialNumber
  | otherwise = Right $ SerialNumber s

mkProductName :: Text -> Either ProductError ProductName
mkProductName s
  | T.length s > 128 = Left $ ProductNameTooLong
  | otherwise = Right $ ProductName s

{- mkProduct :: Text -> Text -> Either ProductError Product
mkProduct serialNumber productName = Product
  <$> mkSerialNumber serialNumber
  <*> mkProductName productName -}

mapError :: (e1 -> e2) -> Either e1 a -> Either e2 a
mapError f = either (Left . f) Right

mkProduct :: Shop -> Text -> Text -> Either ProductError Product
mkProduct shop serialNumber productName = Product
  <$> pure shop
  <*> mkSerialNumber serialNumber
  <*> mkProductName productName