module SchemePrinter where

import SchemeParser

instance Show LispVal where
  show (Atom a)   = a
  show (Number n) = show n
  show (String s) = show s
  show (Bool b)
    | b == True   = "#t"
    | b == False  = "#f"
