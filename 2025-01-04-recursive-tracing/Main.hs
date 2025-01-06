module Main where

import Debug.Trace (trace)


factorial :: (Show a, Num a, Eq a) => a -> a
factorial n
  | n == 0 = 1
  | otherwise = n * factorial (n - 1)


factorial' :: (Show a, Num a, Eq a) => a -> a
factorial' n
  | n == 0 = trace "factorial 0 = 1" 1
  | otherwise =
    let result = n * factorial' (n - 1)
    in trace ("factorial "
              ++ show n
              ++ " = "
              ++ show n
              ++ " * "
              ++ "factorial(" ++ show (n - 1) ++ ")"
              ++ " = "
              ++ show result)
       result


sumList :: (Show a, Num a) => [a] -> a
sumList [] = 0
sumList (x:xs) = x + sumList xs


sumList' :: (Show a, Num a) => [a] -> a
sumList' [] = trace "sumList [] = 0" 0
sumList' (x:xs) =
  trace ("sumList " ++ show (x:xs)) (x + sumList' xs)


quicksort :: (Ord a, Show a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  (quicksort $ filter (<= x) xs) ++ [x] ++ (quicksort $ filter (> x) xs)

quicksort' :: (Ord a, Show a) => [a] -> [a]
quicksort' [] = trace "quicksort [] = []" []
quicksort' (x:xs) =
  let less = filter (<= x) xs
      greater = filter (> x) xs
      sortedLess = quicksort' less
      sortedGreater = quicksort' greater
  in trace ("quicksort " ++ show (x:xs) ++ " = " ++ show (sortedLess ++ [x] ++ sortedGreater))
     (sortedLess ++ [x] ++ sortedGreater)


runFactorial :: IO ()
runFactorial = print $ factorial' 5

runSumList :: IO ()
runSumList = print $ sumList' [1, 2, 3, 4, 5]

runQuicksort :: IO ()
runQuicksort = print $ quicksort' [3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5]

main :: IO ()
main = runFactorial >> runSumList >> runQuicksort
