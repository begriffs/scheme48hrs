module SchemePrinter where

import SchemeParser

pp :: LispVal -> String
pp (Atom a)   = a
pp (Number n) = show n
pp (String s) = show s
pp (Bool b)
  | b == True   = "#t"
  | b == False  = "#f"
