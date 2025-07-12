{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Frontend where

import Control.Lens ((^.))
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Map (Map)
import Language.Javascript.JSaddle (liftJSM, js, js1, jsg)
import Language.Javascript.JSaddle.Types

import Obelisk.Frontend
import Obelisk.Configs
import Obelisk.Route
import Obelisk.Generated.Static

import Reflex.Dom.Core

import Common.Api
import Common.Route
import Common.Types
import Common.Pokemon

import Utils


-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.
frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Obelisk Minimal Example"
      elAttr "script" ("type" =: "application/javascript" <> "src" =: $(static "lib.js")) blank
      --elAttr "link" ("href" =: $(static "main.css") <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
      elAttr "script" ("src" =: "https://cdn.tailwindcss.com") blank
  , _frontend_body = do
      elClass "h1" "text-red" $ text "Let's sort Pokemon!"
      prerender (text "Loading...") $ do
        fetchEvent <- fetchPokemonCardsOnLoad

        (refreshBtnEl, _) <- elClass' "button" "bg-blue-600 hover:bg-blue-700 text-white font-semibold py-2 px-4 rounded" $ text "Refresh"
        (shuffleBtnEl, _) <- elClass' "button" "bg-blue-600 hover:bg-blue-700 text-white font-semibold py-2 px-4 rounded" $ text "Shuffle"

        let refreshBtnClick = domEvent Click refreshBtnEl
            shuffleBtnClick = domEvent Click shuffleBtnEl

        refreshEvent <- fetchPokemonCards refreshBtnClick

        myDyn <- holdDyn [] fetchEvent
        let newEvent = tag (current myDyn) shuffleBtnClick
        -- Expected: Event t [Pokemon]
        -- Actual: Event t (DomEventType (Element EventResult GhcjsDomSpace t) 'ClickTag)
        shuffleEvent <- shufflePokemonCards newEvent

        displayData $ leftmost [shuffleEvent, refreshEvent, fetchEvent]
      return ()
  }

fetchPokemonCardsOnLoad ::
  ( DomBuilder t m
  , MonadIO m
  , MonadJSM (Performable m)
  , PerformEvent t m
  , TriggerEvent t m
  , MonadHold t m
  , PostBuild t m
  ) => m (Event t [Pokemon])
fetchPokemonCardsOnLoad = do
  postBuild <- getPostBuild
  response <- getAndDecode ("/list-pokemon" <$ postBuild)
  return $ fmapMaybe id response


fetchPokemonCards ::
  ( DomBuilder t m
  , MonadIO m
  , MonadJSM (Performable m)
  , PerformEvent t m
  , TriggerEvent t m
  , MonadHold t m
  , PostBuild t m
  ) => Event t () -> m (Event t [Pokemon])
fetchPokemonCards triggerEvent = do
  response <- getAndDecode ("/list-pokemon" <$ triggerEvent)
  return $ fmapMaybe id response


shufflePokemonCards ::
  ( DomBuilder t m
  , MonadIO m
  , MonadJSM (Performable m)
  , PerformEvent t m
  , TriggerEvent t m
  , MonadHold t m
  , PostBuild t m
  ) => Event t [Pokemon] -> m (Event t [Pokemon])
shufflePokemonCards pokemonCards =
  performEvent $ ffor pokemonCards $ liftIO . shuffle






-- take 1 random from current list,
--  append to new list,
--  recurse


  --response <- getAndDecode ("/list-pokemon" <$ triggerEvent)
  --return $ fmapMaybe id response


displayData ::
  ( DomBuilder t m
  , MonadHold t m
  , PostBuild t m
  , MonadFix m
  ) => Event t [Pokemon] -> m ()
displayData eventData = do
  dynList <- holdDyn [] eventData
  elClass "div" "grid gap-4 grid-cols-3 grid-rows-3" $ do
    _ <- simpleList dynList (\item -> displayPokemonCard item)
    return ()


displayPokemonCard ::
  ( DomBuilder t m
  , PostBuild t m
  ) => Dynamic t Pokemon -> m ()
displayPokemonCard pokemon = el "div" $ do
  let pname    = _pokemon_pokemonName <$> pokemon
      imageUri = getImageUrl <$> pname
  elDynAttr "img" (serebiiURIToAttrMap <$> imageUri) blank
  el "h5" $ dynText $ (T.pack . show) <$> pname


serebiiURIToAttrMap :: SerebiiURI -> Map T.Text T.Text
serebiiURIToAttrMap (SerebiiURI uri) = "src" =: (T.pack uri) <> "style" =: "width: 100px;"
