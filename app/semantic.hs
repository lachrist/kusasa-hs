module Semantic (execute) where


import Expression as E
import Kontinuation as K
import Binding
import Store
import Value
import Data
import State
import Transition


import Data.List
import Data.Either
import Control.Applicative
import Text.JSON


crash :: String -> Heap -> Transition
crash str heap = Jump $ Failure (Data $ DString str) heap


concretize :: Store Promise -> Value -> Term
concretize pstore (Data dta) = Right dta
concretize pstore (Symbol addr) = case load addr pstore
                                  of (Result dta) -> Right dta
                                     _ -> Left addr


execute :: State -> Transition
execute (Ongoing (E.Constant dta) env kont heap) = applyKontinuation kont (Data dta) heap
execute (Ongoing (E.Variable id) env kont heap@(bstore, _, _)) = maybe (crash ("no such identifier: "++id) heap) (\val -> applyKontinuation kont val heap) (get id env bstore)
execute (Ongoing (E.Abstraction ids expr) env kont heap) = applyKontinuation kont (Control $ Closure ids expr env) heap
execute (Ongoing (E.Application expr exprs) env kont heap) = Jump $ Ongoing expr env (K.Application exprs [] env kont) heap
execute (Ongoing (E.Block expr1 expr2 exprs) env kont heap) = Jump $ Ongoing expr1 env (K.Block expr2 exprs env kont) heap
execute (Ongoing (E.Assignment id expr) env kont heap) = Jump $ Ongoing expr env (K.Assignment id env kont) heap
execute (Ongoing (E.Definition id expr) env kont (bstore, cstore, pstore)) = Jump $ Ongoing expr env (K.Assignment id env kont) (def id (Data DNull) env bstore, cstore, pstore)
execute (Ongoing (E.Branch expr1 expr2 expr3) env kont heap) = Jump $ Ongoing expr1 env (K.Branch expr2 expr3 env kont) heap
execute state = Jump state


apply :: Control -> [Value] -> Kontinuation -> Heap -> Transition
apply (Primitive str) vals kont heap = applyPrimitive str vals kont heap
apply (Action str) vals kont (bstore, cstore, pstore) = let terms = map (concretize pstore) vals
                                                            (addr, pstore2) = save (Thunk str terms) pstore
                                                        in applyKontinuation kont (Symbol addr) (bstore, cstore, pstore2)
apply (Function str) vals kont heap@(bstore, cstore, pstore) = let terms = map (concretize pstore) vals
                                                               in if null (lefts terms)
                                                                  then applyKontinuation kont (Data $ applyFunction str (rights terms)) heap
                                                                  else let (addr, pstore2) = save (Thunk str terms) pstore
                                                                       in applyKontinuation kont (Symbol addr) (bstore, cstore, pstore2)                                                       
apply (Closure ids expr env) vals kont heap@(bstore, cstore, pstore)
  | length ids /= length vals = crash ("argument number mismatch for closure: "++(show expr)) heap
  | otherwise = let (env2, bstore2) = new (zip ids vals) env bstore
                in Jump $ Ongoing expr env2 kont (bstore2, cstore, pstore)


applyKontinuation :: Kontinuation -> Value -> Heap -> Transition
applyKontinuation (K.Application [] vals _ kont) val heap = case vals++[val]
                                                            of ((Control ctr):args) -> apply ctr args kont heap
                                                               (val:_) -> crash ("value not applicable: "++(encode val)) heap
applyKontinuation (K.Application (expr:exprs) vals env kont) val heap = Jump $ Ongoing expr env (K.Application exprs (vals++[val]) env kont) heap
applyKontinuation (K.Assignment id env kont) val heap@(bstore, cstore, pstore) = maybe (crash ("no such identifier: "++id) heap)
                                                                                       (\bstore2 -> applyKontinuation kont (Data DNull) (bstore2, cstore, pstore))
                                                                                       (set id val env bstore)
applyKontinuation (K.Block expr [] env kont) _ heap = Jump $ Ongoing expr env kont heap
applyKontinuation (K.Block expr1 (expr2:exprs) env kont) _ heap = Jump $ Ongoing expr1 env (K.Block expr2 exprs env kont) heap
applyKontinuation (K.Branch expr1 expr2 env kont) val heap@(_, _, pstore) = case concretize pstore val
                                                                            of (Left symbol) -> Fork symbol expr1 expr2 env kont heap
                                                                               (Right DFalse) -> Jump $ Ongoing expr2 env kont heap
                                                                               _ -> Jump $ Ongoing expr1 env kont heap
applyKontinuation K.Root val heap = Jump $ Success val heap


applyFunction :: String -> [Data] -> Data
-- Type querying
applyFunction "?number?"  ((DNumber _):[]) = DTrue
applyFunction "?number?"  (_:[])           = DFalse
applyFunction "?string?"  ((DString _):[]) = DTrue
applyFunction "?string?"  (_:[])           = DFalse
applyFunction "?boolean?" (DTrue:[])       = DTrue
applyFunction "?boolean?" (DFalse:[])      = DTrue
applyFunction "?boolean?" (_:[])           = DFalse
applyFunction "?null?"    (DNull:[])       = DTrue
applyFunction "?null?"    (_:[])           = DFalse
-- Boolean
applyFunction "?and" (DFalse:_:[]) = DFalse
applyFunction "?and" (_:DFalse:[]) = DFalse
applyFunction "?and" (_:_:[]) = DTrue
applyFunction "?or" (DFalse:DFalse:[]) = DFalse
applyFunction "?or" (_:_:[]) = DTrue
applyFunction "?not" (DFalse:[]) = DTrue
applyFunction "?not" (_:[]) = DFalse
-- Equal
applyFunction "?equal?" (dta1:dta2:[]) = if (dta1 == dta2) then DTrue else DFalse
-- Number
applyFunction "?<"  ((DNumber f1):(DNumber f2):[]) = if (f1 < f2)  then DTrue else DFalse
applyFunction "?<=" ((DNumber f1):(DNumber f2):[]) = if (f1 <= f2) then DTrue else DFalse
applyFunction "?+"  ((DNumber f1):(DNumber f2):[]) = DNumber $ f1+f2
applyFunction "?-"  ((DNumber f1):(DNumber f2):[]) = DNumber $ f1-f2
applyFunction "?*"  ((DNumber f1):(DNumber f2):[]) = DNumber $ f1*f2
applyFunction "?/"  ((DNumber f1):(DNumber f2):[]) = DNumber $ f1/f2
-- String
applyFunction "?data->string" (d:[]) = DString $ encode d
applyFunction "?string->data" ((DString str):[]) = case decode str
                                                   of (Ok dta) -> dta
                                                      (Error _) -> DNull


applyPrimitive :: String -> [Value] -> Kontinuation -> Heap -> Transition
-- success/failure
applyPrimitive "success" (val:[]) _ heap = Jump $ Success val heap
applyPrimitive "failure" (val:[]) _ heap = Jump $ Failure val heap
-- Type querying
applyPrimitive "data?"      ((Data _):[])                  kont heap = applyKontinuation kont (Data DTrue)  heap
applyPrimitive "data?"      ((Symbol _):[])                kont heap = applyKontinuation kont (Data DTrue)  heap
applyPrimitive "data?"      (_:[])                         kont heap = applyKontinuation kont (Data DFalse) heap
applyPrimitive "cons?"      ((Cons _):[])                  kont heap = applyKontinuation kont (Data DTrue)  heap
applyPrimitive "cons?"      (_:[])                         kont heap = applyKontinuation kont (Data DFalse) heap
applyPrimitive "closure?"   ((Control (Closure _ _ _)):[]) kont heap = applyKontinuation kont (Data DTrue)  heap
applyPrimitive "closure?"   (_:[])                         kont heap = applyKontinuation kont (Data DFalse) heap
applyPrimitive "primitive?" ((Control (Primitive _)):[])   kont heap = applyKontinuation kont (Data DTrue)  heap
applyPrimitive "primitive?" (_:[])                         kont heap = applyKontinuation kont (Data DFalse) heap
applyPrimitive "action?"    ((Control (Action _)):[])      kont heap = applyKontinuation kont (Data DTrue)  heap
applyPrimitive "action?"    (_:[])                         kont heap = applyKontinuation kont (Data DFalse) heap
applyPrimitive "function?"  ((Control (Function _)):[])    kont heap = applyKontinuation kont (Data DTrue)  heap
applyPrimitive "function?"  (_:[])                         kont heap = applyKontinuation kont (Data DFalse) heap
-- pair
applyPrimitive "cons"     (val1:val2:[])        kont (bstore, cstore, pstore) = let (addr, cstore2) = save (val1, val2) cstore in applyKontinuation kont (Cons addr) (bstore, cstore2, pstore)
applyPrimitive "car"      ((Cons addr):[])      kont heap@(_, cstore, _)      = let (val, _)  = load addr cstore in applyKontinuation kont val heap
applyPrimitive "cdr"      ((Cons addr):[])      kont heap@(_, cstore, _)      = let (_, val)  = load addr cstore in applyKontinuation kont val heap
applyPrimitive "set-car!" ((Cons addr):val1:[]) kont (bstore, cstore, pstore) = let (_, val2) = load addr cstore in applyKontinuation kont (Cons addr) (bstore, mutate addr (val1, val2) cstore, pstore)
applyPrimitive "set-cdr!" ((Cons addr):val1:[]) kont (bstore, cstore, pstore) = let (val2, _) = load addr cstore in applyKontinuation kont (Cons addr) (bstore, mutate addr (val2, val1) cstore, pstore)
-- Equal
applyPrimitive "*equal?"  (val1:val2:[])        kont heap = applyKontinuation kont (if (val1 == val2) then Data DTrue else Data DFalse) heap
-- Remaining
applyPrimitive str vals kont heap = crash ("primitive application failure: "++str) heap
