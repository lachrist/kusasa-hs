module Value (Environment, Couple, Term, Cons, Symbol, Constraint, Value(Cons, Symbol, Data, Control), Control(Closure, Primitive, Function, Action), Promise(Thunk, Result)) where

import Data
import Promise
import Expression
import Store
import Binding
import JSON

import Data.List
import Text.JSON

type Environment = Address (Binding Value)
type Couple = (Value, Value)
type Cons = Address Couple

-----------
-- Value --
-----------

data Value
  = Cons Cons
  | Symbol Symbol
  | Data Data
  | Control Control
  deriving Eq

instance JSON Value where
  showJSON (Cons addr) = JSObject $ toJSObject [("cons", showJSON addr)]
  showJSON (Symbol addr) = JSObject $ toJSObject [("symbol", showJSON addr)]
  showJSON (Data dta) = JSObject $ toJSObject [("data", showJSON dta)]
  showJSON (Control ctr) = JSObject $ toJSObject [("control", showJSON ctr)]
  readJSON (JSObject obj) = case fromJSObject obj
                            of (("cons", jsv):[]) -> (readJSON jsv) >>= (return . Cons)
                               (("symbol", jsv):[]) -> (readJSON jsv) >>= (return . Symbol)
                               (("data", jsv):[]) -> (readJSON jsv) >>= (return . Data)
                               (("control", jsv):[]) -> (readJSON jsv) >>= (return . Control)

-------------
-- Control --
-------------

data Control
  = Closure [String] Expression Environment
  | Primitive String
  | Function String
  | Action String
  deriving Eq

instance JSON Control where
  showJSON (Closure strs expr env) = JSObject $ toJSObject [("parameters", showJSON strs), ("body", showJSON expr), ("environment", showJSON env)]
  showJSON (Primitive str) = showJSON $ str
  showJSON (Function str) = showJSON $ str
  showJSON (Action str) = showJSON $ str
  readJSON (JSString jss) = case fromJSString jss
                            of name@('?':_) -> Ok $ Function name
                               name@('!':_) -> Ok $ Action name
                               name         -> Ok $ Primitive name
  readJSON (JSObject obj) = let pairs = fromJSObject obj
                            in do params <- property "parameters" pairs
                                  body <- property "body" pairs
                                  env <- property "environment" pairs
                                  return $ Closure params body env
  readJSON jsv = Error $ (show jsv)++" is not a valid control"
