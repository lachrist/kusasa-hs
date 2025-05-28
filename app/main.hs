{-# LANGUAGE OverloadedStrings #-}

module Main where

-- http://happstack.com/docs/crashcourse/index.html
-- https://hackage.haskell.org/package/happstack-server
-- http://happstack.com/docs/happstack-server-7.0.1/doc/html/happstack-server/Happstack-Server-Response.html

import Data.ByteString.Char8 (ByteString, pack, unpack)
import Data.ByteString.Lazy.Char8 (ByteString, pack, unpack)

import Semantic

import Text.JSON

import Data.Maybe (isJust, fromJust)

import Control.Monad (msum, guard)
import Happstack.Server (
  ServerPart,
  Response,
  Method(GET, POST),
  Browsing(EnableBrowsing),
  method,
  nullConf,
  simpleHTTP,
  serveDirectory,
  toResponseBS,
  takeRequestBody,
  unBody,
  askRq)

main :: IO ()
main = simpleHTTP nullConf $ msum [dynamic, static]

static :: ServerPart Response
static = do method GET
            serveDirectory EnableBrowsing [] "./client"

dynamic :: ServerPart Response
dynamic = do method POST
             req <- askRq
             rqbody <- takeRequestBody req
             guard (isJust rqbody)
             return $ toResponseBS "application/json" (run $ unBody (fromJust rqbody))

run :: Data.ByteString.Lazy.Char8.ByteString -> Data.ByteString.Lazy.Char8.ByteString
run bstr = case decode $ Data.ByteString.Lazy.Char8.unpack bstr
           of (Ok st) -> Data.ByteString.Lazy.Char8.pack $ encode $ execute st
              (Error msg) -> Data.ByteString.Lazy.Char8.pack $ encode msg
