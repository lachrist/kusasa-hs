module Promise (Symbol, Constraint, Term, Promise(Thunk, Result)) where

import Data
import JSON
import Store

import Text.JSON

type Symbol = Address Promise
type Constraint = (Symbol, Promise)
type Term = Either Symbol Data

data Promise
  = Thunk String [Term]
  | Result Data

instance JSON Promise where
  showJSON (Thunk str terms) = JSObject $ toJSObject [("function", showJSON str), ("arguments", JSArray $ map showJSON terms)]
  showJSON (Result dta) = showJSON dta
  readJSON (JSObject obj) = let pairs = fromJSObject obj
                            in do fct <- property "function" pairs
                                  args <- property "arguments" pairs
                                  return $ Thunk fct args
  readJSON jsv = (readJSON jsv) >>= (return . Result)
