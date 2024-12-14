{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}

module Backend where

import qualified Data.Text as T
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL

import Snap.Core (Snap, writeLBS, modifyResponse, setContentType)
import Network.HTTP.Client (Response)
import Network.HTTP.Simple (httpJSON, getResponseBody, parseRequest)
import Data.Text.Encoding
import Data.Aeson (ToJSON, Value, (.:), withObject)
import Data.Aeson.Types (parseEither, Parser)
import Control.Monad.IO.Class (liftIO)
import Control.Exception (try, SomeException)

import Common.Types
import Common.Route
import Obelisk.Route
import Obelisk.Backend


backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> serve $ \case
    BackendRoute_Missing :/ () -> pure ()
    BackendRoute_CryptoPrices :/ () -> handleCryptoPrices
  , _backend_routeEncoder = fullRouteEncoder
  }

symbols :: [CryptoSymbol]
symbols = [
  CryptoSymbol "BTCUSD" "bitcoin" "usd",
  CryptoSymbol "ETHUSD" "ethereum" "usd",
  CryptoSymbol "WLDUSD" "worldcoin-wld" "usd",
  CryptoSymbol "SOLUSD" "solana" "usd"
  ]

handleCryptoPrices :: Snap ()
handleCryptoPrices = do
  cryptoData <- liftIO $ fetchCryptoData symbols
  case cryptoData of
    Left err -> do
      modifyResponse $ setContentType "text/plain"
      writeLBS $ BL.fromStrict $ encodeUtf8 $ T.pack $ "Error: " <> err
    Right cryptos -> writeJsonResponse cryptos


writeJsonResponse :: ToJSON a => a -> Snap ()
writeJsonResponse value = do
  modifyResponse $ setContentType "application/json"
  writeLBS $ Aeson.encode value


fetchCryptoData :: [CryptoSymbol] -> IO (Either String [CryptoData])
fetchCryptoData cryptoSymbols = do
  let symbolNames = map symbolName cryptoSymbols
      url = "https://api.coingecko.com/api/v3/simple/price?ids="
          <> T.unpack (T.intercalate "," (T.pack <$> symbolNames))
          <> "&vs_currencies=" <> "usd"
  request <- parseRequest url
  result <- try (httpJSON request) :: IO (Either SomeException (Response Value))
  case result of
    Left _ -> do
      return $ Left $ "Failed to fetch data for symbols."
    Right response -> do
      let body = getResponseBody response :: Value
      return $ parseCoinGeckoResponse cryptoSymbols body

parseCoinGeckoResponse :: [CryptoSymbol] -> Value -> Either String [CryptoData]
parseCoinGeckoResponse cryptoSymbols value =
  parseEither parseResponse value
  where
    parseResponse :: Value -> Parser [CryptoData]
    parseResponse = withObject "CoinGecko Response" $ \obj -> do
      mapM (parseCrypto obj) cryptoSymbols

    parseCrypto :: KM.KeyMap Value -> CryptoSymbol -> Parser CryptoData
    parseCrypto obj cryptoSymb = do
      let key = symbolName cryptoSymb
      case KM.lookup (Key.fromString key) obj of
        Nothing -> fail $ "Symbol not found in response: " <> key
        Just val -> do
          priceObj <- withObject "CryptoData" return val
          cPrice <- priceObj .: "usd"
          return $ CryptoData cryptoSymb cPrice
