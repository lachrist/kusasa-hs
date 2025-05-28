
module Data (Data(DNull, DTrue, DFalse, DNumber, DString), parser) where

import Text.ParserCombinators.Parsec
import Text.JSON

data Data
  = DNull
  | DTrue
  | DFalse
  | DNumber Float
  | DString String
  deriving Eq

-------------------
-- Show & Parsec --
-------------------

instance Show Data where
  show DNull  = "#null"
  show DTrue  = "#t"
  show DFalse = "#f"
  show (DNumber float) = show float
  show (DString string) = show string

parser = choice [try dstring, try dnumber, try dnull, try dfalse, try dtrue]

dnull = (string "#null") >> (return DNull)
dtrue = (string "#t") >> (return DTrue)
dfalse = (string "#f") >> (return DFalse)

dstring = do char '"'
             str <- many (noneOf "\"")
             char '"'
             return $ DString str

dnumber = do sign <- try (char '+') <|> try (char '-') <|> return ' '
             part1 <- many1 digit
             char '.'
             part2 <- many1 digit
             return $ DNumber $ read $ (sign:part1)++('.':part2)

----------
-- JSON --
----------

instance JSON Data where
  showJSON DNull = JSNull
  showJSON DTrue = JSBool True
  showJSON DFalse = JSBool False
  showJSON (DNumber float) = showJSON float
  showJSON (DString str) = showJSON str
  readJSON JSNull = Ok DNull
  readJSON (JSBool True) = Ok DTrue
  readJSON (JSBool False) = Ok DFalse
  readJSON (JSRational _ rat) = Ok $ DNumber $ fromRational rat
  readJSON (JSString jss) = Ok $ DString $ fromJSString jss
