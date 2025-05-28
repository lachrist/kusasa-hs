module Binding (Binding, new, get, def, set) where

import qualified Control.Applicative as Applicative
import Store
import Text.JSON

data Binding a
  = Root
  | Frame [(String, a)] (Address (Binding a))

new :: [(String, a)] -> Address (Binding a) -> Store (Binding a) -> (Address (Binding a), Store (Binding a))
new pairs addr store = save (Frame pairs addr) store

get :: String -> Address (Binding a) -> Store (Binding a) -> Maybe a
get str env store = case load env store of
  Root -> Nothing
  (Frame pairs parent) -> (lookup str pairs) Applicative.<|> (get str parent store)

def :: String -> a -> Address (Binding a) -> Store (Binding a) -> Store (Binding a)
def str val addr store =
  let (Frame pairs parent) = load addr store
      pairs2 = maybe ((str, val) : pairs) id (update str val pairs [])
   in mutate addr (Frame ((str, val) : pairs) parent) store

set :: String -> a -> Address (Binding a) -> Store (Binding a) -> Maybe (Store (Binding a))
set str val addr store = case load addr store of
  Root -> Nothing
  (Frame pairs parent) ->
    maybe
      (set str val parent store)
      (\pairs -> Just $ mutate addr (Frame pairs parent) store)
      (update str val pairs [])

----------
-- JSON --
----------

instance (JSON a) => JSON (Binding a) where
  showJSON Root = JSNull
  showJSON (Frame pairs addr) = JSObject $ toJSObject $ ("(parent)", showJSON addr) : (map (\(str, x) -> (str, showJSON x)) pairs)
  readJSON JSNull = Ok Root
  readJSON (JSObject obj) =
    maybe
      (Error $ "JSON parse error: frame: " ++ show (obj) ++ " does not define (parent) does not exists")
      ( \(jsv, pairs) -> do
          addr <- readJSON jsv
          pairs <- mapM (\(str, jsv) -> (readJSON jsv) >>= (\x -> return (str, x))) pairs
          return $ Frame pairs addr
      )
      (extract "(parent)" (fromJSObject obj) [])
  readJSON jsv = Error $ "JSON parse error: Binding can either be an object or null, got: " ++ (show jsv)

-----------------
-- Pair helper --
-----------------

update :: (Eq a) => a -> b -> [(a, b)] -> [(a, b)] -> Maybe [(a, b)]
update _ _ [] _ = Nothing
update a b ((a1, b1) : pairs1) pairs2
  | a == a1 = Just $ ((a1, b) : pairs1) ++ pairs2
  | otherwise = update a b pairs1 ((a1, b1) : pairs2)

extract :: (Eq a) => a -> [(a, b)] -> [(a, b)] -> Maybe (b, [(a, b)])
extract _ [] _ = Nothing
extract a ((a1, b1) : pairs1) pairs2
  | a == a1 = Just $ (b1, pairs1 ++ pairs2)
  | otherwise = extract a pairs1 ((a1, b1) : pairs2)
