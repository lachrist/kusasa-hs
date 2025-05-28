module State (Heap, State(Ongoing, Success, Failure)) where

import Value
import Store
import Binding
import Expression
import Kontinuation
import JSON

import Control.Applicative
import Text.JSON

type Heap = (Store (Binding Value), Store Couple, Store Promise)

data State
  = Ongoing Expression Environment Kontinuation Heap
  | Success Value Heap
  | Failure Value Heap

instance JSON State where
  showJSON (Ongoing expr env kont heap) = JSObject $ toJSObject [("expression", showJSON expr), ("environment", showJSON env), ("kontinuation", showJSON kont), ("heap", showJSON heap)]
  showJSON (State.Success val heap) = JSObject $ toJSObject [("success", showJSON val), ("heap", showJSON heap)]
  showJSON (State.Failure val heap) = JSObject $ toJSObject [("failure", showJSON val), ("heap", showJSON heap)]
  readJSON (JSObject obj) = let pairs = fromJSObject obj
                            in do val <- property "success" pairs
                                  heap <- property "heap" pairs
                                  return $ Success val heap
                               <|> do val <- property "failure" pairs
                                      heap <- property "heap" pairs
                                      return $ Failure val heap
                                   <|> do expr <- property "expression" pairs
                                          env <- property "environment" pairs
                                          kont <- property "kontinuation" pairs
                                          heap <- property "heap" pairs
                                          return $ Ongoing expr env kont heap
  readJSON jsv = Error $ "expects an object for stae, got: "++(show jsv)

