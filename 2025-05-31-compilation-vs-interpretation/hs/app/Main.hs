{-# OPTIONS_GHC -Wall -Werror #-}

module Main where

import System.Random (randomRIO)


-- Example 1
newtype User = User { _user_name :: String }

getUserName :: Maybe User -> String
getUserName (Just user) = _user_name user
getUserName Nothing = "Unknown"

simulateGetUser :: IO (Maybe User)
simulateGetUser = do
  randomVal <- randomRIO (1, 10)
  let user = if randomVal > (5::Int) then Just $ User "Alice" else Nothing
  pure user  

main2 :: IO ()
main2 = do
    user <- simulateGetUser
    putStrLn $ getUserName user

-- Example 2
data Shape 
    = Circle Double 
    | Square Double
    | Triangle Double Double Double
    deriving Show

getArea :: Shape -> Double
getArea (Circle r) = pi * r * r
getArea (Square s) = s * s
getArea (Triangle a b c) = heronArea a b c

heronArea :: Double -> Double -> Double -> Double
heronArea a b c =
  let s = (a + b + c) / 2
  in sqrt (s * (s - a) * (s - b) * (s - c))


-- Example 3
data Data = Data { value :: Int }
processData :: Data -> Int
processData d = value d * 2


{- 
someFunc :: Maybe Int -> String
someFunc (Just i) = show i
 -}


-- Example 4
firstElement :: [a] -> Maybe a
firstElement (x:_) = Just x
firstElement [] = Nothing



main :: IO ()
main = do
    let shape1 = Triangle 2.0 3.4 2.1    
    main2
    putStrLn $ "Area = " <> show (getArea shape1)
    putStrLn $ "ProcessData 3 = " <> (show $ processData $ Data 3)

    let x = myfunc ["ab", "def"]
    putStrLn $ "B=" <> show x

    pure ()

g :: String -> Int
g = length

f :: Int -> Bool
f = odd

myfunc :: [String] -> [Bool]
myfunc xs =   
   map (f . g) xs 


-- map f (map g xs) ==


