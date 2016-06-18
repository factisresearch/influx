{-# LANGUAGE OverloadedStrings #-}
module Database.Influx
  ( createDatabase
  , Config (..)
  ) where

import Network.HTTP.Simple
import qualified Data.ByteString.Lazy.Char8 as L8

data Config = Config
  { configCreds  :: !Credentials
  , configServer :: !L8.ByteString
  , configManager :: !Manager
  }

createDatabase :: Config
               -> L8.ByteString
               -> IO (Either String ())
createDatabase config name = do
  let request
      = setRequestMethod "POST"
      . setRequestQueryString [("q", Just ("CREATE DATABASE " `L8.append`  name))]
      <$> parseUrl (configServer config)
