{-# LANGUAGE OverloadedStrings #-}

module Database.Influx
    ( ping
    , Credentials(..)
    , Config(..)
    , InfluxVersion(..)
    ) where

import Network.HTTP.Client.Conduit
import Network.HTTP.Simple
import Data.ByteString ()
import Data.Text (Text ())
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy.Char8 as B

-- | User credentials
data Credentials
    = Credentials
    { credsUser :: !Text
    , credsPassword :: !Text
    } deriving (Show)

data Config = Config
    { configCreds  :: !(Maybe Credentials)
    , configServer :: !String
    , configManager :: !Manager
    }

newtype RetentionPolicy = RetentionPolicy { unRetentionPolicy :: Text }

data EpochPrecision
    = Hours
    | Minutes
    | Seconds
    | Milliseconds
    | Microseconds
    | Nanoseconds
    deriving Eq

data OptionalParams = OptionalParams
    { chunkSize       :: !(Maybe Int)
    , epoch           :: !(Maybe EpochPrecision)
    , pretty          :: !(Maybe Bool)
    , retentionPolicy :: !(Maybe RetentionPolicy)
    }

newtype InfluxVersion
    = InfluxVersion
    { unInfluxVersion :: Text
    } deriving Show

newtype Query
    = Query
    { unQuery :: Text
    } deriving Show

urlAppend :: String -> String -> String
urlAppend base path = base' ++ "/" ++ path'
  where base' = if last base == '/' then init base else base
        path' = if head path == '/' then tail path else path

ping :: Config
     -> IO (Maybe InfluxVersion)
ping config =
    do request <- setRequestMethod "HEAD" <$> parseUrl (urlAppend (configServer config) "/ping")
       response <- httpLBS request
       let version = getResponseHeader "X-Influxdb-Version" response
       return $
           if null version || getResponseStatusCode response /= 204
             then Nothing
             else Just . InfluxVersion . T.decodeUtf8 $ head version
