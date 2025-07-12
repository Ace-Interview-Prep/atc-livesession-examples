module Common.Types where

import System.Random
import Data.List

import Common.Pokemon (Pokemon, mkPokemon)


newtype SerebiiURI = SerebiiURI { _getURI :: String
                                }


data PokemonCard = PokemonCard {
  _pokemonCard_num :: Int,
  _pokemonCard_pokemon :: Pokemon,
  _pokemonCard_year :: Int,
  _pokemonCard_image :: SerebiiURI
  }


newtype PokemonDeck = PokemonDeck [PokemonCard]

