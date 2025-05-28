module JSON (property, parsec) where

import Text.JSON
import Text.Parsec.Prim
import Text.ParserCombinators.Parsec

property :: (JSON a) => String -> [(String, JSValue)] -> Result a
property str pairs = maybe (Text.JSON.Error $ "no such key " ++ str) readJSON (lookup str pairs)

parsec :: Parsec String () a -> JSString -> Result a
parsec parser jss = either (Text.JSON.Error . show) Text.JSON.Ok (parse parser "(unknown)" (fromJSString jss))
