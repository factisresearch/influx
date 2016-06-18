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
  , configServer :: !String
  , configManager :: !Manager
  }

newtype InfluxVersion = InfluxVersion { unInfluxVersion :: Text }
  deriving Show

urlAppend :: String -> String -> String
urlAppend base path = base ++ "/" ++ path
  where base = if last base = '/' then init base else base
        path = if head path = '/' then tail path else path

ping :: Config
     -> IO InfluxVersion
ping config = do
  request <- setRequestMethod "HEAD" <$> parseUrl (urlAppend (configServer config) "/ping")
  response <- httpLBS request
  let version = getResponseHeader "X-Influxdb-Version" response
  return $ if (not . null) version || getResponseStatus response == 204 
    then Nothing
    else Just (head version)


