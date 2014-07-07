module Store (Address, Store, mutate, save, load) where

import Text.JSON

newtype Store x = S [x]
newtype Address x = A Int deriving Eq

mutate :: (Address x) -> x -> Store x -> Store x
mutate (A i) x (S xs) = let (xs1, (_:xs2)) = splitAt i xs
                           in S $ xs1++(x:xs2)

save :: x -> Store x -> (Address x, Store x)
save x (S xs) = (A $ length xs, S $ xs++[x])

load :: Address a -> Store a -> a
load (A i) (S xs) = xs !! i

instance JSON x => JSON (Store x) where
  showJSON (S xs) = showJSON xs
  readJSON (JSArray xs) = (mapM readJSON xs) >>= (return . S)
  readJSON jsv = Error $ (show jsv)++" invalid for address"

instance JSON (Address x) where
  showJSON (A i) = JSRational True (toRational i)
  readJSON (JSRational _ rat) = Ok $ A $ round rat
  readJSON jsv = Error $ (show jsv)++" invalid for address"
