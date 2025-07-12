{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Common.Pokemon where


import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)


data Pokemon = Pokemon PokemonName [PokemonType] deriving (Show, Eq, Generic)

instance ToJSON Pokemon
instance FromJSON Pokemon


_pokemon_pokemonName :: Pokemon -> PokemonName
_pokemon_pokemonName (Pokemon n _) = n


mkPokemon :: PokemonName -> [PokemonType] -> Either String Pokemon
mkPokemon pokeName pokeType =
  Right $ Pokemon pokeName pokeType


data PokemonType
  = Normal
  | Fire
  | Water
  | Grass
  | Electric
  | Ice
  | Fighting
  | Poison
  | Ground
  | Flying
  | Fairy
  | Psychic
  | Bug
  | Rock
  | Steel
  | Ghost
  | Dragon
  deriving (Show, Eq, Generic)


instance ToJSON PokemonType
instance FromJSON PokemonType


data PokemonName
  = Bulbasaur
  | Ivysaur
  | Venusaur
  | Charmander
  | Charmeleon
  | Charizard
  | Squirtle
  | Wartortle
  | Blastoise
  | Caterpie
  | Metapod
  | Butterfree
  | Weedle
  | Kakuna
  | Beedrill
  | Pidgey
  | Pidgeotto
  | Pidgeot
  | Rattata
  | Raticate
  | Spearow
  | Fearow
  | Ekans
  | Arbok
  | Pikachu
  | Raichu
  | Sandshrew
  | Sandslash
  | NidoranF
  | Nidorina
  | Nidoqueen
  | NidoranM
  | Nidorino
  | Nidoking
  | Clefairy
  | Clefable
  | Vulpix
  | Ninetales
  | Jigglypuff
  | Wigglytuff
  | Zubat
  | Golbat
  | Oddish
  | Gloom
  | Vileplume
  | Paras
  | Parasect
  | Venonat
  | Venomoth
  | Diglett
  | Dugtrio
  | Meowth
  | Persian
  | Psyduck
  | Golduck
  | Mankey
  | Primeape
  | Growlithe
  | Arcanine
  | Poliwag
  | Poliwhirl
  | Poliwrath
  | Abra
  | Kadabra
  | Alakazam
  | Machop
  | Machoke
  | Machamp
  | Bellsprout
  | Weepinbell
  | Victreebel
  | Tentacool
  | Tentacruel
  | Geodude
  | Graveler
  | Golem
  | Ponyta
  | Rapidash
  | Slowpoke
  | Slowbro
  | Magnemite
  | Magneton
  | Farfetchd
  | Doduo
  | Dodrio
  | Seel
  | Dewgong
  | Grimer
  | Muk
  | Shellder
  | Cloyster
  | Gastly
  | Haunter
  | Gengar
  | Onix
  | Drowzee
  | Hypno
  | Krabby
  | Kingler
  | Voltorb
  | Electrode
  | Exeggcute
  | Exeggutor
  | Cubone
  | Marowak
  | Hitmonlee
  | Hitmonchan
  | Lickitung
  | Koffing
  | Weezing
  | Rhyhorn
  | Rhydon
  | Chansey
  | Tangela
  | Kangaskhan
  | Horsea
  | Seadra
  | Goldeen
  | Seaking
  | Staryu
  | Starmie
  | MrMime
  | Scyther
  | Jynx
  | Electabuzz
  | Magmar
  | Pinsir
  | Tauros
  | Magikarp
  | Gyarados
  | Lapras
  | Ditto
  | Eevee
  | Vaporeon
  | Jolteon
  | Flareon
  | Porygon
  | Omanyte
  | Omastar
  | Kabuto
  | Kabutops
  | Aerodactyl
  | Snorlax
  | Articuno
  | Zapdos
  | Moltres
  | Dratini
  | Dragonair
  | Dragonite
  | Mewtwo
  | Mew
  deriving (Show, Eq, Enum, Generic, Bounded)

instance ToJSON PokemonName
instance FromJSON PokemonName



kantoPokemon :: [Pokemon]
kantoPokemon = [ (Pokemon Bulbasaur [Grass, Poison])
               , (Pokemon Ivysaur [Grass, Poison])
               , (Pokemon Venusaur [Grass, Poison])
               , (Pokemon Charmander [Fire])
               , (Pokemon Charmeleon [Fire])
               , (Pokemon Charizard [Fire, Flying])
               , (Pokemon Squirtle [Water])
               , (Pokemon Wartortle [Water])
               , (Pokemon Blastoise [Water])
               , (Pokemon Caterpie [Bug])
                  , (Pokemon Metapod [Bug])
                  , (Pokemon Butterfree [Bug, Flying])
                  , (Pokemon Weedle [Bug, Poison])
                  , (Pokemon Kakuna [Bug, Poison])
                  , (Pokemon Beedrill [Bug, Poison])
                  , (Pokemon Pidgey [Normal, Flying])
                  , (Pokemon Pidgeotto [Normal, Flying])
                  , (Pokemon Pidgeot [Normal, Flying])
                  , (Pokemon Rattata [Normal])
                  , (Pokemon Raticate [Normal])
                  , (Pokemon Spearow [Normal, Flying])
                  , (Pokemon Fearow [Normal, Flying])
                  , (Pokemon Ekans [Poison])
                  , (Pokemon Arbok [Poison])
                  , (Pokemon Pikachu [Electric])
                  , (Pokemon Raichu [Electric])
                  , (Pokemon Sandshrew [Ground])
                  , (Pokemon Sandslash [Ground])
                  , (Pokemon NidoranF [Poison])
                  , (Pokemon Nidorina [Poison])
                  , (Pokemon Nidoqueen [Poison, Ground])
                  , (Pokemon NidoranM [Poison])
                  , (Pokemon Nidorino [Poison])
                  , (Pokemon Nidoking [Poison, Ground])
                  , (Pokemon Clefairy [Fairy])
                  , (Pokemon Clefable [Fairy])
                  , (Pokemon Vulpix [Fire])
                  , (Pokemon Ninetales [Fire])
                  , (Pokemon Jigglypuff [Normal, Fairy])
                  , (Pokemon Wigglytuff [Normal, Fairy])
                  , (Pokemon Zubat [Poison, Flying])
                  , (Pokemon Golbat [Poison, Flying])
                  , (Pokemon Oddish [Grass, Poison])
                  , (Pokemon Gloom [Grass, Poison])
                  , (Pokemon Vileplume [Grass, Poison])
                  , (Pokemon Paras [Bug, Grass])
                  , (Pokemon Parasect [Bug, Grass])
                  , (Pokemon Venonat [Bug, Poison])
                  , (Pokemon Venomoth [Bug, Poison])
                  , (Pokemon Diglett [Ground])
                  , (Pokemon Dugtrio [Ground])
                  , (Pokemon Meowth [Normal])
                  , (Pokemon Persian [Normal])
                  , (Pokemon Psyduck [Water])
                  , (Pokemon Golduck [Water])
                  , (Pokemon Mankey [Fighting])
                  , (Pokemon Primeape [Fighting])
                  , (Pokemon Growlithe [Fire])
                  , (Pokemon Arcanine [Fire])
                  , (Pokemon Poliwag [Water])
                  , (Pokemon Poliwhirl [Water])
                  , (Pokemon Poliwrath [Water, Fighting])
                  , (Pokemon Abra [Psychic])
                  , (Pokemon Kadabra [Psychic])
                  , (Pokemon Alakazam [Psychic])
                  , (Pokemon Machop [Fighting])
                  , (Pokemon Machoke [Fighting])
                  , (Pokemon Machamp [Fighting])
                  , (Pokemon Bellsprout [Grass, Poison])
                  , (Pokemon Weepinbell [Grass, Poison])
                  , (Pokemon Victreebel [Grass, Poison])
                  , (Pokemon Tentacool [Water, Poison])
                  , (Pokemon Tentacruel [Water, Poison])
                  , (Pokemon Geodude [Rock, Ground])
                  , (Pokemon Graveler [Rock, Ground])
                  , (Pokemon Golem [Rock, Ground])
                  , (Pokemon Ponyta [Fire])
                  , (Pokemon Rapidash [Fire])
                  , (Pokemon Slowpoke [Water, Psychic])
                  , (Pokemon Slowbro [Water, Psychic])
                  , (Pokemon Magnemite [Electric, Steel])
                  , (Pokemon Magneton [Electric, Steel])
                  , (Pokemon Farfetchd [Normal, Flying])
                  , (Pokemon Doduo [Normal, Flying])
                  , (Pokemon Dodrio [Normal, Flying])
                  , (Pokemon Seel [Water])
                  , (Pokemon Dewgong [Water, Ice])
                  , (Pokemon Grimer [Poison])
                  , (Pokemon Muk [Poison])
                  , (Pokemon Shellder [Water])
                  , (Pokemon Cloyster [Water, Ice])
                  , (Pokemon Gastly [Ghost, Poison])
                  , (Pokemon Haunter [Ghost, Poison])
                  , (Pokemon Gengar [Ghost, Poison])
                  , (Pokemon Onix [Rock, Ground])
                  , (Pokemon Drowzee [Psychic])
                  , (Pokemon Hypno [Psychic])
                  , (Pokemon Krabby [Water])
                  , (Pokemon Kingler [Water])
                  , (Pokemon Voltorb [Electric])
                  , (Pokemon Electrode [Electric])
                  , (Pokemon Exeggcute [Grass, Psychic])
                  , (Pokemon Exeggutor [Grass, Psychic])
                  , (Pokemon Cubone [Ground])
                  , (Pokemon Marowak [Ground])
                  , (Pokemon Hitmonlee [Fighting])
                  , (Pokemon Hitmonchan [Fighting])
                  , (Pokemon Lickitung [Normal])
                  , (Pokemon Koffing [Poison])
                  , (Pokemon Weezing [Poison])
                  , (Pokemon Rhyhorn [Ground, Rock])
                  , (Pokemon Rhydon [Ground, Rock])
                  , (Pokemon Chansey [Normal])
                  , (Pokemon Tangela [Grass])
                  , (Pokemon Kangaskhan [Normal])
                  , (Pokemon Horsea [Water])
                  , (Pokemon Seadra [Water])
                  , (Pokemon Goldeen [Water])
                  , (Pokemon Seaking [Water])
                  , (Pokemon Staryu [Water])
                  , (Pokemon Starmie [Water, Psychic])
                  , (Pokemon MrMime [Psychic, Fairy])
                  , (Pokemon Scyther [Bug, Flying])
                  , (Pokemon Jynx [Ice, Psychic])
                  , (Pokemon Electabuzz [Electric])
                  , (Pokemon Magmar [Fire])
                  , (Pokemon Pinsir [Bug])
                  , (Pokemon Tauros [Normal])
                  , (Pokemon Magikarp [Water])
                  , (Pokemon Gyarados [Water, Flying])
                  , (Pokemon Lapras [Water, Ice])
                  , (Pokemon Ditto [Normal])
                  , (Pokemon Eevee [Normal])
                  , (Pokemon Vaporeon [Water])
                  , (Pokemon Jolteon [Electric])
                  , (Pokemon Flareon [Fire])
                  , (Pokemon Porygon [Normal])
                  , (Pokemon Omanyte [Rock, Water])
                  , (Pokemon Omastar [Rock, Water])
                  , (Pokemon Kabuto [Rock, Water])
                  , (Pokemon Kabutops [Rock, Water])
                  , (Pokemon Aerodactyl [Rock, Flying])
                  , (Pokemon Snorlax [Normal])
                  , (Pokemon Articuno [Ice, Flying])
                  , (Pokemon Zapdos [Electric, Flying])
                  , (Pokemon Moltres [Fire, Flying])
                  , (Pokemon Dratini [Dragon])
                  , (Pokemon Dragonair [Dragon])
                  , (Pokemon Dragonite [Dragon, Flying])
                  , (Pokemon Mewtwo [Psychic])
                  , (Pokemon Mew [Psychic])
                  ]
