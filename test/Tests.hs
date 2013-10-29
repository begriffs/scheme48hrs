module Main where

import Test.Framework (defaultMain)
import SchemeParser.Test

main :: IO ()
main = defaultMain [parserSuite]
