{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Frontend where

import Control.Monad.Fix
import qualified Data.Text as T
import Language.Javascript.JSaddle (MonadJSM)

import Obelisk.Frontend
import Obelisk.Route
import Obelisk.Generated.Static

import Reflex.Dom.Core

import Common.Route
import Common.Types


frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "ATC Crypto Example"
      elAttr "script" ("type" =: "application/javascript" <> "src" =: $(static "lib.js")) blank
      elAttr "script" ("src" =: "https://cdn.tailwindcss.com") blank
  , _frontend_body = do
      elClass "div" "px-6 pt-6" $ do
        elClass "h1" "text-2xl font-bold" $ text "Let's trade crypto!"
        elClass "p" "italic pb-6" $ text "This example will demonstrate how we can write a webpage to display crypto price information."
        
        (refreshElement, _) <- elClass' "button" "bg-blue-500 hover:bg-blue-600 text-white font-bold py-2 px-4 rounded shadow-lg" $
          text "Refresh Prices"

        let refreshEvent = domEvent Click refreshElement

        elClass "div" "pt-6" $ do
          elClass "h2" "pb-2 text-lg font-bold" $ text "Crypto Prices"
          prerender_ (return ()) $ do
            cryptoDataDyn <- fetchCryptoPrices refreshEvent
            displayCryptoTable cryptoDataDyn

      pure ()
  }


fetchCryptoPrices
  :: ( DomBuilder t m
     , TriggerEvent t m
     , PerformEvent t m
     , MonadJSM (Performable m)
     , MonadHold t m
     )
  => Event t ()
  -> m (Dynamic t [CryptoData])
fetchCryptoPrices refreshEvent = do
  let url = "/crypto-prices"
  responseEvent <- performRequestAsync $ fmap (const $ xhrRequest "GET" url def) refreshEvent
  holdDyn [] $ fmapMaybe decodeXhrResponse responseEvent


displayCryptoTable
  :: ( DomBuilder t m
     , MonadHold t m
     , PostBuild t m
     , MonadFix m
     )
  => Dynamic t [CryptoData]
  -> m ()
displayCryptoTable cryptoDataDyn = do
  let headerRow = ["Symbol", "Currency", "Price"]
  _ <- elClass "table" "table-auto border-collapse border border-gray-300 rounded-lg shadow-lg w-full text-left" $ do
    _ <- el "thead" $ do
      elClass "tr" "bg-gray-100" $
        mapM (elClass "th" "px-4 py-2 border-b border-gray-300 font-bold text-gray-700" . text) headerRow
    el "tbody" $ simpleList cryptoDataDyn renderCryptoRow
  pure ()


renderCryptoRow :: (DomBuilder t m, PostBuild t m) => Dynamic t CryptoData -> m ()
renderCryptoRow cryptoDyn = do
  elClass "tr" "hover:bg-gray-100" $ do
    elClass "td" "px-4 py-2 border-t border-gray-300" $ dynText $ T.pack . symbol . cryptoSymbol <$> cryptoDyn
    elClass "td" "px-4 py-2 border-t border-gray-300" $ dynText $ T.pack . currency . cryptoSymbol <$> cryptoDyn
    elClass "td" "px-4 py-2 border-t border-gray-300" $ dynText $ T.pack . ("$" <>) . show . price <$> cryptoDyn
  pure ()
