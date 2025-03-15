module Main where

import Animal

parseInt :: String -> Either String Int
parseInt s = case reads s :: [(Int, String)] of
  [(n, "")] -> Right n
  _ -> Left "Invalid integer"


validateAge :: Int -> Either String Int
validateAge n
  | n >= 0 && n <= 120 = Right n
  | otherwise = Left "Age out of range"


processAge :: String -> Either String Int
processAge input = do
  n <- parseInt input
  validateAge n


isPrime :: Int -> Bool
isPrime n = n > 1 && null [x | x <- [2..floor $ sqrt $ fromIntegral n], n `mod` x == 0]

primeFactors :: Int -> [Int]
primeFactors n = [x | x <- [2..n], n `mod` x == 0, isPrime x]





myWings :: Either Error NumberOfWings
myWings = mkNumberOfWings (2)


mkDog :: Position -> Either Error Animal
mkDog pos = do
  numLegs <- mkNumberOfLegs 4
  pure $ Dog Bark (Walking numLegs) pos

mkCat :: Position -> Either Error Animal
mkCat pos = do
  numLegs <- mkNumberOfLegs 4
  pure $ Cat Meow (Walking numLegs) pos

mkBird :: Position -> Either Error Animal
mkBird pos = do
  numWings <- mkNumberOfWings 2
  pure $ Bird Chirp (Flying numWings) pos

mkSpider :: Position -> Either Error Animal
mkSpider pos = do
  numLegs <- mkNumberOfLegs 8
  pure $ Spider Tsk (Walking numLegs) pos

mkWhale :: Position -> Either Error Animal
mkWhale pos = do
  numFins <- mkNumberOfFins 2
  pure $ Whale AWOOO (Swimming numFins) pos


move :: Animal -> Position -> Animal
move (Dog sound movetype _) newPos = Dog sound movetype newPos
move (Cat sound movetype _) newPos = Cat sound movetype newPos
move (Bird sound movetype _) newPos = Bird sound movetype newPos
move (Spider sound movetype _) newPos = Spider sound movetype newPos
move (Whale sound movetype _) newPos = Whale sound movetype newPos


speak :: Animal -> AnimalSound
speak (Dog sound _ _) = sound
speak (Cat sound _ _) = sound
speak (Bird sound _ _) = sound
speak (Spider sound _ _) = sound
speak (Whale sound _ _) = sound



createCat :: Either Error Animal
createCat = do
  position <- mkPosition 1 2
  cat <- mkCat position
  pure $ cat


main :: IO ()
main = do
  case createCat of
    Left err -> putStrLn $ show err
    Right cat ->
        putStrLn $ show $ speak cat
