{-# LANGUAGE NoImplicitPrelude #-}
-- | Note: we dont need to do this wall of imports if we remove this
-- extension
--
-- we could also just do:
  -- import Prelude
-- this will re-export all the modules below
-- 
--
-- but we manually import them here so that we can see where it comes from and can search them on hoogle: https://hoogle.haskell.org/

import Data.Foldable
import Data.Function (on, (.))
import Data.Char (toUpper)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Ord (Ord)
import Data.Eq (Eq, (==))
import Data.List
  (sort, reverse, filter, map, words)
import Data.Bool (Bool, (&&) )
import Data.Char (Char, toLower)
import System.IO (IO, putStrLn)
import Prelude
  ( String, Int, IO, undefined
  , putStrLn, show, otherwise, mod
  )



-- | Agenda
--
--  1) Common interview questions, in Haskell
--     -> how they evaluate
--     -> how lambda calculus makes everything a first-class value
--  2) Working with structure
--     -> Map's
--     -> retaining context through Functor 
--  3) Using then ignoring structure
--     -> Monads
--  4) How we use functional typeclasses to overload/abstract












isPalindrome :: String -> Bool
isPalindrome str = str == reverse str

-- >>> isPalindrome "racecar"
-- True

-- >>> isPalindrome "hello world!"
-- False

fizzle :: Int -> String
fizzle n
  | n `mod` 3 == 0 && n `mod` 5 == 0 = "Fizz Buzz!"
  | n `mod` 3 == 0 = "Fizz!"
  | n `mod` 5 == 0 = "Buzz!"
  | otherwise = show n

fizzbuzz100 :: IO ()
fizzbuzz100 = do
    for_ [1..100] (putStrLn . fizzle)

isAnagram :: String -> String -> Bool
isAnagram = (==) `on` (sort . map toLower)

-- >>> isAnagram "elbow" "below"
-- True
-- >>> isAnagram "bored" "road"
-- False
-- >>> isAnagram "stressed" "desserts"
-- True

simpleMinMax :: Ord a => [a] -> (a, a)
simpleMinMax xs = (minimum xs, maximum xs)

myMaximum :: a
myMaximum = undefined

myMinimum :: a
myMinimum = undefined

safeMinMax :: a
safeMinMax = undefined

fastSafeMinMax :: a
fastSafeMinMax = undefined  

-- | purposely incorrect (uncomment)
capitalizeWords :: String -> String
capitalizeWords = unwords . map capitalize . words
  where
    --capitalize :: String -> [String]
    capitalize (x:xs) = toUpper x : xs
    capitalize [] = []


countElem :: Eq a => a -> [a] -> Int
countElem x = length . filter (== x)

lines :: String -> [String]
lines = undefined

unwords :: [String] -> String
unwords = undefined

-- | From external **untrusted, object-oriented, dynamically-typed API 
students :: [(String, Int)]
students =
  [ ("Alice", 85)
  , ("Bob", 42)
  , ("Charlie", 77)
  , ("Diana", 93)
  , ("Eve", 59)
  , ("", 90)
  , ("SELECT * FROM STUDENT_NAMES where password == 'pass1234'", 40)
  , ("Rick", 201)
  , ("Morty", -4)
  ]

-- Make them into a Map
studentsMap :: [(String, Int)] -> Map String Int
studentsMap = undefined

-- calculate letter grade

data Grade = F | D | C | B | A
assignLetterGradeNaive :: Int -> Grade
assignLetterGradeNaive = undefined

assignLetterGrade :: Int -> Maybe Grade
assignLetterGrade = undefined

giveStudentsGrades :: Map String Int -> Map String Grade
giveStudentsGrades = undefined


lookupValidGrade :: String -> Maybe Grade
lookupValidGrade str = undefined
  -- does name exist?
  -- is it a valid name?
  -- is it a valid number?
  

type Name = String

-- What if we want to know why we failed?
  -- lets use Either



-- | What classes are we using here?

-- | class Show a where
--      show :: a -> String

-- | class Monad (m :: * -> *) where
--      (>>=) :: m a
--            -> (a -> m b)
--            -> m b

-- | data Either err x = Left err | Right x
-- | data Maybe x = Nothing | Just x

-- | instance Monad (Either err) ...
-- | instance Monad (Maybe) ...
--
-- | Even completely separate functionality:
--
-- instance Monad (State s) ...
-- instance Monad (IO)




-- If time:
  -- list comprehension
  -- Data.NonEmpty.Class: https://hackage.haskell.org/package/non-empty-0.3.5/docs/Data-NonEmpty-Class.html#v:sort
  -- Why all of this allows for an infinitely customizable language
     --https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-Type-Bool.html#t:-38--38-
