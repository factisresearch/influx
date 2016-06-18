{-# LANGUAGE OverloadedStrings #-}
module Database.Influx
  ( ping
  , Config (..)
  , InfluxVersion ()
  ) where

import Network.HTTP.Simple
import Data.Text (Text ())
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as B

data Config = Config
  { configCreds  :: !Credentials
  , configServer :: !B.ByteString
  , configManager :: !Manager
  }

newtype InfluxVersion = InfluxVersion { unInfluxVersion :: Text }
  deriving Show

ping :: Config
     -> IO InfluxVersion
ping config = do
  request <- setRequestMethod "HEAD" <$> parseUrl (configServer config)
  response <- httpLBS request
  let version = getResponseHeader "X-Influxdb-Version" response
  return $ if (not . null) version || getResponseStatus response == 204 
    then Nothing
    else Just (head version)
