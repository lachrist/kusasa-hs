{-# LANGUAGE OverloadedStrings #-}

module Main where

-- http://happstack.com/docs/crashcourse/index.html
-- https://hackage.haskell.org/package/happstack-server
-- http://happstack.com/docs/happstack-server-7.0.1/doc/html/happstack-server/Happstack-Server-Response.html

import Control.Monad (guard, msum)
import Data.ByteString.Char8 (ByteString, pack, unpack)
import Data.ByteString.Lazy.Char8 (ByteString, pack, unpack)
import Data.Maybe (fromJust, isJust)
import Happstack.Server
  ( Browsing (EnableBrowsing),
    Method (GET, POST),
    Response,
    ServerPart,
    askRq,
    method,
    nullConf,
    serveDirectory,
    simpleHTTP,
    takeRequestBody,
    toResponseBS,
    unBody,
  )
import Semantic
import Text.JSON

main :: IO ()
main = do
  putStrLn "Starting server on port 8000..."
  simpleHTTP nullConf $ msum [dynamic, static]

static :: ServerPart Response
static = do
  method GET
  serveDirectory EnableBrowsing [] "./client"

dynamic :: ServerPart Response
dynamic = do
  method POST
  req <- askRq
  rqbody <- takeRequestBody req
  guard (isJust rqbody)
  return $ toResponseBS "application/json" (run $ unBody (fromJust rqbody))

run :: Data.ByteString.Lazy.Char8.ByteString -> Data.ByteString.Lazy.Char8.ByteString
run bstr = case decode $ Data.ByteString.Lazy.Char8.unpack bstr of
  (Ok st) -> Data.ByteString.Lazy.Char8.pack $ encode $ execute st
  (Error msg) -> Data.ByteString.Lazy.Char8.pack $ encode msg
