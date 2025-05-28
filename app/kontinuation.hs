module Kontinuation (Kontinuation (Application, Assignment, Block, Branch, Root)) where

import Expression
import JSON
import Text.JSON
import Value

data Kontinuation
  = Application [Expression] [Value] Environment Kontinuation
  | Assignment String Environment Kontinuation
  | Block Expression [Expression] Environment Kontinuation
  | Branch Expression Expression Environment Kontinuation
  | Root

instance JSON Kontinuation where
  showJSON (Kontinuation.Application exprs vals env kont) = JSObject $ toJSObject $ [("tag", showJSON "application"), ("todo", showJSON vals), ("done", showJSON exprs), ("environment", showJSON env), ("parent", showJSON kont)]
  showJSON (Kontinuation.Assignment str env kont) = JSObject $ toJSObject $ [("tag", showJSON "assignment"), ("identifier", showJSON str), ("environment", showJSON env), ("parent", showJSON kont)]
  showJSON (Kontinuation.Block expr exprs env kont) = JSObject $ toJSObject $ [("tag", showJSON "block"), ("head", showJSON expr), ("tail", showJSON exprs), ("environment", showJSON env), ("parent", showJSON kont)]
  showJSON (Kontinuation.Branch expr1 expr2 env kont) = JSObject $ toJSObject $ [("tag", showJSON "branch"), ("consequent", showJSON expr1), ("alternative", showJSON expr2), ("environment", showJSON env), ("parent", showJSON kont)]
  showJSON Root = JSNull
  readJSON JSNull = Ok Root
  readJSON (JSObject obj) =
    let pairs = fromJSObject obj
     in (property "tag" pairs) >>= (\tag -> parse tag pairs)

parse :: String -> [(String, JSValue)] -> Result Kontinuation
parse "application" pairs = do
  todo <- property "todo" pairs
  done <- property "done" pairs
  env <- property "environment" pairs
  parent <- property "parent" pairs
  return $ Kontinuation.Application done todo env parent
parse "assignment" pairs = do
  id <- property "identifier" pairs
  env <- property "environment" pairs
  parent <- property "parent" pairs
  return $ Kontinuation.Assignment id env parent
parse "block" pairs = do
  head <- property "head" pairs
  tail <- property "tail" pairs
  env <- property "environment" pairs
  parent <- property "parent" pairs
  return $ Kontinuation.Block head tail env parent
parse "branch" pairs = do
  cons <- property "consequent" pairs
  alt <- property "alternative" pairs
  env <- property "environment" pairs
  parent <- property "parent" pairs
  return $ Kontinuation.Branch cons alt env parent
parse tag _ = Error $ "invalid tag: " ++ tag
