module Animal (
  Position, mkPosition,
  NumberOfLegs, mkNumberOfLegs,
  NumberOfWings, mkNumberOfWings,
  NumberOfFins, mkNumberOfFins,
  AnimalSound (..), mkAnimalSound,
  MovementType (..),
  Animal (..),
  Error
) where

newtype Error = Error String deriving Show
newtype Position = MkPosition (Int, Int) deriving Show


mkPosition :: Int -> Int -> Either Error Position
mkPosition x y
  | x < 0 || x > 10 = Left $ Error "Out of bounds on the x-axis"
  | y < 0 || y > 10 = Left $ Error "Out of bounds on the y-axis"
  | otherwise = Right $ MkPosition (x, y)


newtype NumberOfLegs = MkNumberOfLegs Int deriving Show

mkNumberOfLegs :: Int -> Either Error NumberOfLegs
mkNumberOfLegs n
  | n < 1 = Left $ Error "Must have at least 2 legs"
  | otherwise = Right $ MkNumberOfLegs n


newtype NumberOfWings = MkNumberOfWings Int deriving Show

mkNumberOfWings :: Int -> Either Error NumberOfWings
mkNumberOfWings n
  | n <= 0 = Left $ Error "Must have wings"
  | n `mod` 2 /= 0 = Left $ Error "Must have an even number of wings"
  | otherwise = Right $ MkNumberOfWings n


newtype NumberOfFins = MkNumberOfFins Int deriving Show

mkNumberOfFins :: Int -> Either Error NumberOfFins
mkNumberOfFins n
  | n < 0 = Left $ Error "Must have fins"
  | otherwise = Right $ MkNumberOfFins n


data AnimalSound
  = Bark
  | Meow
  | Tsk
  | Chirp
  | AWOOO
  deriving Show

mkAnimalSound :: String -> Either Error AnimalSound
mkAnimalSound str
  | str == "bark" = Right $ Bark
  | str == "meow" = Right $ Meow
  | str == "tsk" = Right $ Tsk
  | str == "chirp" = Right $ Chirp
  | str == "awooo" = Right $ AWOOO
  | otherwise = Left $ Error "Not a recognized animal sound"


----


data MovementType
  = Walking NumberOfLegs
  | Flying NumberOfWings
  | Swimming NumberOfFins
  deriving Show

data Animal
  = Dog AnimalSound MovementType Position
  | Cat AnimalSound MovementType Position
  | Spider AnimalSound MovementType Position
  | Bird AnimalSound MovementType Position
  | Whale AnimalSound MovementType Position
  deriving Show
