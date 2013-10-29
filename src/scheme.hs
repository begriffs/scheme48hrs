module Main where

import System.Environment
import Text.ParserCombinators.Parsec (parse)
import SchemeParser

main :: IO ()
main = do
  args <- getArgs
  putStrLn $ readExpr (args !! 0)

readExpr :: String -> String
readExpr input = case parse SchemeParser.parseExpr "lisp" input of
  Left err -> "No Match: " ++ show err
  Right val -> show val
