{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

--module Shop (Shop, ShopNumber, mkShop) where
module Shop where

import Data.Text hiding (elem)
import Data.Char
import GHC.Generics (Generic)
import Data.Aeson
import Data.Aeson.Key (fromString)
import qualified Data.Text as T
import TypeClasses

data ShopError
  = ShopNumberWrongLength
  | ShopNumberNotNumeric Text
  | LocationCodeWrongLength
  | InvalidLocationCode Text
  | ShopNameExceedsMaxLength Int

instance Show ShopError where
  show ShopNumberWrongLength = "The store number must be exactly four numeric digits in length."
  show (ShopNumberNotNumeric num) = "The store number (" ++ T.unpack num ++ ") must consist of only numeric digits."
  show LocationCodeWrongLength = "The location code must be exactly two characters in length."
  show (InvalidLocationCode code) = "The location code you provided (" ++ T.unpack code ++ ") does not exist."
  show (ShopNameExceedsMaxLength len) = "The store name must be less than 32 characters. The name entered was " ++ show len ++ " characters long."

data Shop = Shop
  { shopNumber :: ShopNumber
  , shopLocationCode :: LocationCode
  , shopName :: ShopName
  } deriving Generic

instance Eq Shop where
  (==) (Shop n1 l1 _) (Shop n2 l2 _) =
    n1 == n2 && l1 == l2

instance ToJSON Shop
instance FromJSON Shop where
  parseJSON = withObject "Shop" $ \o -> do
    res <- mkShop
      <$> o .: fromString "shopNumber"
      <*> o .: fromString "shopLocationCode"
      <*> o .: fromString "shopName"
    case res of
      Right shop -> pure shop
      Left e -> fail $ show e

newtype ShopNumber =
  ShopNumber Text
  deriving (ToJSON, FromJSON, Generic, Eq, Show)

instance IsText ShopNumber where
  getText (ShopNumber sn) = sn

data LocationCode =
   NL | PE | NS | NB | QC | ON | MB | SK | AB | BC | YT | NT | NU
   deriving (ToJSON, FromJSON, Generic, Eq, Show, Read, Bounded, Enum)

fromText :: Text -> Maybe LocationCode
fromText t = case reads (unpack t) of
  [(lc, "")] -> Just lc
  _          -> Nothing

instance IsText LocationCode where
   getText NL = T.pack "NL"
   getText PE = T.pack "PE"
   getText NS = T.pack "NS"
   getText NB = T.pack "NB"
   getText QC = T.pack "QC"
   getText ON = T.pack "ON"
   getText MB = T.pack "MB"
   getText SK = T.pack "SK"
   getText AB = T.pack "AB"
   getText BC = T.pack "BC"
   getText YT = T.pack "YT"
   getText NT = T.pack "NT"
   getText NU = T.pack "NU"

newtype ShopName =
  ShopName Text
  deriving (ToJSON, FromJSON, Generic, Eq, Show)

instance IsText ShopName where
  getText (ShopName sn) = sn

mkShopNumber :: Text -> Either ShopError ShopNumber
mkShopNumber num
  | T.length num /= 4 = Left $ ShopNumberWrongLength
  | elem False $ fmap isNumber (T.unpack num) = Left $ ShopNumberNotNumeric num
  | otherwise = Right $ ShopNumber num

mkLocationCode :: Text -> Either ShopError LocationCode
mkLocationCode lc
  | T.length lc /= 2 = Left $ LocationCodeWrongLength
  | otherwise = maybe (Left $ InvalidLocationCode lc) Right (fromText lc)

mkShopName :: Text -> Either ShopError ShopName
mkShopName name
  | T.length name > 32 = Left $ ShopNameExceedsMaxLength $ T.length name
  | otherwise = Right $ ShopName name

mkShop :: Text -> Text -> Text -> (Either ShopError Shop)
mkShop num loc name = Shop 
  <$> mkShopNumber num
  <*> mkLocationCode loc
  <*> mkShopName name