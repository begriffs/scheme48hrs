module SchemeParser.Test where

import Test.Framework (testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Control.Monad (liftM)

import Text.ParserCombinators.Parsec (parse)
import SchemeParser
import SchemePrinter

instance Arbitrary LispVal where
  arbitrary =
    oneof [ liftM  Atom   arbitrary
          , liftM  Number arbitrary
          , liftM  String arbitrary
          , liftM  Bool   arbitrary ]

parserSuite :: Test
parserSuite = testGroup "Scheme Parser"
   [ testProperty "Show is the inverse of Parse" prop_showParseInverse ]

prop_showParseInverse :: LispVal -> Bool
prop_showParseInverse tree = case (parse parseExpr "lisp" (pp tree)) of
  Left  _      -> True
  Right parsed -> parsed == tree
