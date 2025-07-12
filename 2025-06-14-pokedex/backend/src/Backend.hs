{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Backend where

import Snap
import Common.Route
import Obelisk.Route
import Obelisk.Backend
import qualified Data.Aeson as A
import Common.Pokemon

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> serve $ \case
      BackendRoute_ListPokemon :/ () -> handleListPokemon
      BackendRoute_Missing :/ () -> return ()
  , _backend_routeEncoder = fullRouteEncoder
  }


handleListPokemon :: Snap ()
handleListPokemon = do
  -- let list = [ (Pokemon Bulbasaur [Grass])
  --            , (Pokemon Squirtle [Water])
  --            , (Pokemon Charmander [Fire])
  --            ]
  writeLBS $ A.encode kantoPokemon
