{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Common.Types where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)

data CryptoSymbol = CryptoSymbol
  { symbol :: String
  , symbolName :: String
  , currency :: String
  } deriving stock (Show, Eq, Generic)

instance FromJSON CryptoSymbol
instance ToJSON CryptoSymbol

data CryptoData = CryptoData
  { cryptoSymbol :: CryptoSymbol
  , price :: Double
  } deriving stock (Show, Eq, Generic)

instance FromJSON CryptoData
instance ToJSON CryptoData
