module SchemeParser where

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Numeric (readHex, readOct, readDec)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>#^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many $ noneOf "\"\\" <|> escapedChars
  char '"'
  return $ String x

parseBoolean :: Parser LispVal
parseBoolean = do
  char '#'
  (char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False))

escapedChars :: Parser Char
escapedChars = do
  char '\\'
  x <- oneOf "\"\\nrt"
  return $ case x of
    'n' -> '\n'
    'r' -> '\r'
    't' -> '\t'
    _   -> x

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many $ letter <|> digit <|> symbol
  return $ Atom ([first] ++ rest)

parseNumber :: Parser LispVal
parseNumber = return . Number . read =<< many1 digit

parseBase :: Char -> Parser Char -> (String -> Integer) -> Parser LispVal
parseBase delim validDigit reader = do
  try $ string ['#', delim]
  return . Number . reader =<< many1 validDigit

parseExpr :: Parser LispVal
parseExpr = parseBoolean
        <|> parseString
        <|> parseAtom
        <|> parseNumber
        <|> parseBase 'x' hexDigit (r readHex)
        <|> parseBase 'd' digit    (r readDec)
        <|> parseBase 'o' octDigit (r readOct)
  where r = (id fst .) . ((!! 0) .)
