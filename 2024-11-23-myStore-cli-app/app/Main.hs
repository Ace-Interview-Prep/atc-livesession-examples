module Main where

import Text.Parsec
import Data.Functor.Identity

-- A store
--   -> sells clothign (tshirts, shoes, pants)
     -- > diff features (size, color, etc) 

-- Credit Card
-- card number: 16 digits : 1234 1234 1234 1234
-- expiry date ( MM / YY ) : 06 / 29
-- security code: 3 digits : 123

-- "myStore (shirt|pants|shoes) (size) (color) 1234123412341234 0629 123" -> Card 

-- shirts -> shirt sizing
-- shoes -> shoe sizing
-- pants -> waist size and length

-- RED One
case1 :: Parsecc FullItem
case1 = do
  size <- shoeSize
  _ <- space
  c <- parseColor 
  pure $ Shoes' size 

case2 :: Parsecc FullItem
case2 = do
  c <- parseColor
  _ <- space
  size <- shoeSize
  pure $ Shoes' size

parseFullItem :: PurchaseItem -> Parsecc FullItem
parseFullItem itemName = case itemName of
  Shoes -> do
    (try case1) <|> case2 
    -- size <- shoeSize
    -- _ <- space
    -- c <- parseColor 
    -- pure $ Shoes' size 
  Shirt -> do
    size <- shirtSize
    pure $ Shirt' size    
  Pants -> do
    (heightSize,waistSize) <- pantSize
    pure $ Pants' waistSize heightSize 
  
parseColor :: Parsecc String
parseColor = string "red" 


shirtSize :: Parsecc ShirtSizing
shirtSize = (SM <$ (string "sm"))
  <|> (MD <$ (try $ string "md"))
  <|> (LG <$ (try $ string "lg")) 

shoeSize :: Parsecc ShoeSizing
shoeSize = (One <$ (string "one"))
  <|> (Two <$ (try $ string "two"))
  <|> (Three <$ (try $ string "three")) 

-- (+) :: Int -> Int -> Int 
-- Parsec Int -> Parsec Int -> Parsec Int

-- Input: "28 32" 
pantSize :: Parsecc (HeightSizing, WaistSizing)
pantSize = do
  h <- heightSize
  _ <- space 
  w <- waistSize
  pure (h, w) 
  
--
waistSize :: Parsecc WaistSizing
waistSize = (W_28 <$ (string "28"))
  <|> (W_30 <$ (try $ string "30"))
  <|> (W_32 <$ (try $ string "32")) 

heightSize :: Parsecc HeightSizing
heightSize = (H_28 <$ (string "28"))
  <|> (H_30 <$ (try $ string "30"))
  <|> (H_32 <$ (try $ string "32")) 
--

data FullItem
  = Shoes' ShoeSizing
  | Shirt' ShirtSizing
  | Pants' WaistSizing HeightSizing
  deriving Show 
  
data Card = Card
  { cardNum :: [Char]
  , expiry :: [Char]
  , cvcNum :: [Char] 
  } deriving Show

data PurchaseItem = Shoes | Shirt | Pants deriving Show

data ShirtSizing = SM | MD | LG deriving Show
data ShoeSizing = One | Two | Three deriving Show 
data PantsSizing = PantsSizing WaistSizing HeightSizing deriving Show 

data WaistSizing = W_28 | W_30 | W_32 deriving Show
data HeightSizing = H_28 | H_30 | H_32 deriving Show 

data Color = Red | Blue | Green deriving Show

type Parsecc a = ParsecT String () Identity a 

main :: IO ()
main = do
  putStrLn "start!"
  input <- getLine
  case parse myParser "bashInput" input of
    Left err -> putStrLn $ "Got error: " ++ (show err)
    Right output -> putStrLn $  "Success: " ++ (show output)


myParser :: Parsecc (FullItem, Card)
myParser = do
  _ <- exe
  _ <- space 
  item <- storeItem
  _ <- space
  fullItem <- parseFullItem item
  _ <- space -- "myStore ..."
  cNum <- cardNumber
  _ <- space
  exp <- expiryDate
  _ <- space
  cvc_ <- cvc
  pure $ (fullItem, Card cNum exp cvc_)


-- (<$) :: a -> Parsecc b -> Parsecc a
-- Shirt <$ (string "shirt")

-- Parsecc "shirt"
-- Parsecc Shirt

storeItem :: Parsecc PurchaseItem
storeItem =
  (Shirt <$ (try $ string "shirt"))
  <|> (Shoes <$ (try $ string "shoes"))
  <|> (Pants <$ (string "pants"))

exe :: Parsecc String
exe = string "myStore" 

cvc :: Parsecc [Char]   
cvc = count 3 digit

expiryDate :: Parsecc [Char]
expiryDate = count 4 digit

cardNumber :: Parsecc [Char]
cardNumber = count 16 digit



