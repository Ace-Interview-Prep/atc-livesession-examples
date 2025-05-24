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
import Data.Function (on, (.), ($))
import Data.Char (toUpper)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Ord (Ord, max, min)
import Data.Eq (Eq, (==))
import Data.List
  (sort, reverse, filter, map, words)
import Data.Bool (Bool(..), (&&) )
import Data.Char (Char, toLower)
import System.IO (IO, putStrLn)
import Prelude
  ( String, Int, IO, undefined
  , putStrLn, show, otherwise, rem
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

-- (\x -> x + 1) 2
-- >>> 2+1
-- >>> 3 






-- instance Eq Bool where
--   True == False = False
--   True == True  = True
--   False == False = True

-- -- python-like
-- def f(myInput :: Int | Float | String):
--   if typeof(myInput) == string:
--      convertToInt 
--   let x = statement1()
--   statement2(x)
--   statement2() -- crash  
--   let output = statement3()

--   if x:
--     then
--        g = someFunc()
--        doA
--     else
--       func(g)
--       doB 
--   return output


-- f :: Maybe ()
-- f = do
--   x <- statement1
--   statement2 x
--   statement2 -- fail to typecheck
--   statement3

-- | A function is a type that takes an input
-- | and output as type parameters
isPalindrome :: String -> Bool
isPalindrome str = str == (reverse str)

-- f $ g x 
value :: Bool
value = True  

isValueTrue :: Bool
isValueTrue {--} = True == value 

-- >>> isPalindrome "racecar"
-- True

-- >>> isPalindrome "hello world!"
-- False

-- [ ................................................... ] 
-- idx ++
-- idx_moveBack = idx
-- idx_moveBack = idx_moveBack - 1 
-- idx_moveBack = idx 


-- rem n 3 == n `rem` 3

result1 = (==) True False
result2 = True == False 

-- (==) :: Bool -> Bool -> Bool
-- (==) 

fizzle :: Int -> String
fizzle n
  | rem n 3 == 0 && n `rem` 5 == 0 = "Fizz Buzz!"
  -- ((n `rem` 3) == 0) && ((n `rem` 5) == 0) = "Fizz Buzz!"
  | n `rem` 3 == 0 = "Fizz!"
  | n `rem` 5 == 0 = "Buzz!"
  | otherwise = show n

fizzbuzz100 :: IO ()
fizzbuzz100 = do
  for_ [1..100] (\x -> putStrLn . fizzle $ x)

-- | For homework, how does this evaluate
isAnagram :: String -> String -> Bool
isAnagram str1 str2 = ((==) `on` (sort . map toLower)) str1 str2



-- >>> isAnagram "elbow" "below"
-- True
-- >>> isAnagram "bored" "road"
-- False
-- >>> isAnagram "stressed" "desserts"
-- True

simpleMinMax :: Ord a => [a] -> (a, a)
simpleMinMax xs = (minimum xs, maximum xs)


-- add1 x = x + 1

-- do
--   add1 3
--   add1 3

-- xs :: [Int] 
-- xs = [1..]

-- print $ take 5 xs

-- [1..10]
-- 1 : 2 : 3 : 4 : 5 : 6 : 7 : 8 : 9 : 10 : []
-- xs
-- x : xs
-- [1]

-- 1 : []
-- 1 : (2 : 3 : 4 : 5 : 6 : 7 : 8 : 9 : 10 : [])
-- x : xs 

-- take :: Int -> [a] -> [a]
-- take 0 _ = [] 
-- take n (x:xs) = x : take (n - 1) xs

-- silly :: a -> a
-- silly xs = xs 

-- 1 : 2 : 3 : xs
-- 1 : 2 : 3 : 4 : []

-- myMaximum [] == -1

-- output * someOtherOutput
-- r = -7234235
-- = p - r
-- =
-- = 65

-- 71 71 71 72 71.5 73 80 73 73 73
-- 80
-- 5 000 000
-- = 400 000 000

-- data Maybe a = Just a | Nothing

-- myMaximum [] == Nothing
-- myMaximum [] == 0

-- fromMaybe 0 


myMaximum :: Ord a => [a] -> Maybe a
myMaximum [] = Nothing
myMaximum (x:xs) = Just $ go x xs
  where
    go :: Ord a => a -> [a] -> a
    go currMax [] = currMax
    go currMax (x:xs) =
      go (max currMax x) (xs) 

  -- let currMax = max x1 x2
myMinimum :: Ord a => [a] -> Maybe a
myMinimum [] = Nothing
myMinimum (x:xs) = Just $ go x xs
  where
    go :: Ord a => a -> [a] -> a
    go currMax [] = currMax
    go currMax (x:xs) =
      go (min currMax x) (xs) 

myOrd :: Ord a
  => (a -> a -> a) -> [a] -> Maybe a
myOrd f [] = Nothing
myOrd f (x:xs) = Just $ go f x xs
  where
    go :: Ord a
      => (a -> a -> a)
      -> a
      -> [a]
      -> a
    go f currM [] = currM
    go f currM (x:xs) =
      go f (f currM x) (xs) 



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
