module Transition (Transition(Jump, Fork)) where

import State
import Value
import JSON
import Expression
import Binding
import Kontinuation

import Text.JSON

data Transition
  = Jump State
  | Fork Symbol Expression Expression Environment Kontinuation Heap

instance JSON Transition where
  showJSON (Jump st) = JSObject $ toJSObject [("next", showJSON st)]
  showJSON (Fork sym expr1 expr2 env kont heap) = JSObject $ toJSObject [("fork", showJSON sym), ("consequent", showJSON expr1), ("alternative", showJSON expr2), ("environment", showJSON env), ("kontinuation", showJSON kont), ("heap", showJSON heap)]
  readJSON (JSObject obj) = case fromJSObject obj
                            of (("next", jsv):[]) -> (readJSON jsv) >>= (return . Jump)
                               pairs -> do fork <- property "fork" pairs
                                           expr1 <- property "consequent" pairs
                                           expr2 <- property "alternative" pairs
                                           env <- property "environment" pairs
                                           kont <- property "kontinuation" pairs
                                           heap <- property "heap" pairs
                                           return $ Fork fork expr1 expr2 env kont heap
  readJSON jsv = Error $ "Transition expects an object, got "++(show jsv)
