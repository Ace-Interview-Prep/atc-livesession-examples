module Parser where

import Control.Monad.State
import Data.Char (isDigit)

-- Parser state
data ParseState = ParseState
  { input :: String
  , position :: Int
  } deriving (Show)

-- State monad for parsing
type Parser a = State ParseState a

-- Consume a character
consume :: Parser (Maybe Char)
consume = do
  ParseState inp pos <- get
  case inp of
    [] -> return Nothing
    (c:cs) -> do
      put $ ParseState cs (pos + 1)
      return (Just c)

-- Parse a digit
parseDigit :: Parser (Maybe Int)
parseDigit = do
  mc <- consume
  case mc of
    Just c | isDigit c -> return (Just (read [c]))
    _ -> return Nothing

-- Parse a number (multiple digits)
parseNumber :: Parser (Maybe Int)
parseNumber = do
  digits <- many parseDigit
  let validDigits = [d | Just d <- digits]
  return $ if null validDigits
           then Nothing
           else Just (foldl (\n d -> n * 10 + d) 0 validDigits)

-- Helper: Repeat a parser
many :: Parser (Maybe a) -> Parser [Maybe a]
many p = do
  mx <- p
  case mx of
    Nothing -> return []
    Just _ -> do
      rest <- many p
      return (mx : rest)

-- Run parser
runParser :: Parser a -> String -> (a, ParseState)
runParser p input = runState p (ParseState input 0)

-- Example
exampleParse :: (Maybe Int, ParseState)
exampleParse = runParser parseNumber "123abc"
