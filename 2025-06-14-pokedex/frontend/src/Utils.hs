module Utils where

import System.Random
import Common.Pokemon
import Common.Types
import Text.Printf (printf)


getImageUrl :: PokemonName -> SerebiiURI
getImageUrl pname = SerebiiURI $ "https://www.serebii.net/pokemon/art/" ++ (printf "%03d" ((fromEnum pname) + 1)) ++ ".png"


shuffle :: [a] -> IO [a]
shuffle [] = pure []
shuffle xs = do
  picked <- randomRIO (1, length xs)
  let (before, after) = splitAt picked xs
  xs' <- shuffle $ after <> tail before
  pure $ head before : xs'
