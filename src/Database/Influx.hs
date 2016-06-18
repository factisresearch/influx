{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Influx
    ( Credentials(..)
    , Config(..)
    , EpochPrecision(..)
    , RetentionPolicy
    , DatabaseName
    , OptionalParams(..)
    , defaultOptParams
    , InfluxVersion(..)
    , ping
    , Query(..)
    , getQueryRaw
    ) where

import Control.Arrow (second)
import Data.String (IsString)
import Data.Maybe (catMaybes)
import Data.Text (Text ())
import Network.HTTP.Client.Conduit
import Network.HTTP.Simple
import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

-- | User credentials
data Credentials
    = Credentials
    { credsUser :: !Text
    , credsPassword :: !Text
    } deriving (Show)

credsToQueryString :: Credentials -> [(B.ByteString, Maybe B.ByteString)]
credsToQueryString creds =
    fmap (second Just) $
    [ ("u", T.encodeUtf8 (credsUser creds))
    , ("p", T.encodeUtf8 (credsPassword creds))
    ]

data Config = Config
    { configCreds  :: !(Maybe Credentials)
    , configServer :: !String
    , configManager :: !Manager
    }

type RetentionPolicy = Text

data EpochPrecision
    = Hours
    | Minutes
    | Seconds
    | Milliseconds
    | Microseconds
    | Nanoseconds
    deriving (Eq, Show)

epochToBytestring :: EpochPrecision -> B.ByteString
epochToBytestring epoch =
    case epoch of
      Hours -> "h"
      Minutes -> "m"
      Seconds -> "s"
      Milliseconds -> "ms"
      Microseconds -> "us"
      Nanoseconds -> "ns"

type DatabaseName = Text

data OptionalParams
    = OptionalParams
    { optChunkSize :: !(Maybe Int)
    , optEpoch :: !(Maybe EpochPrecision)
    , optRetentionPolicy :: !(Maybe RetentionPolicy)
    , optDatabase :: !(Maybe DatabaseName)
    } deriving (Show)

defaultOptParams :: OptionalParams
defaultOptParams =
    OptionalParams
    { optChunkSize = Nothing
    , optEpoch = Nothing
    , optRetentionPolicy = Nothing
    , optDatabase = Nothing
    }

optParamsToQueryString :: OptionalParams -> [(B.ByteString, Maybe B.ByteString)]
optParamsToQueryString opts =
    fmap (second Just) $
    catMaybes
    [ (,) "chunk_size" . T.encodeUtf8 . T.pack . show <$> optChunkSize opts
    , (,) "epoch" . epochToBytestring <$> optEpoch opts
    , (,) "rp" . T.encodeUtf8 <$> optRetentionPolicy opts
    , (,) "db" . T.encodeUtf8 <$> optDatabase opts
    ]

newtype InfluxVersion
    = InfluxVersion
    { unInfluxVersion :: Text
    } deriving Show

newtype Query
    = Query
    { unQuery :: Text
    } deriving (Show, IsString)

urlAppend :: String -> String -> String
urlAppend base path = base' ++ "/" ++ path'
  where base' = if last base == '/' then init base else base
        path' = if head path == '/' then tail path else path

ping :: Config -> IO (Maybe InfluxVersion)
ping config =
    do request <- setRequestMethod "HEAD" <$> parseUrl (urlAppend (configServer config) "/ping")
       response <- httpLBS request
       let version = getResponseHeader "X-Influxdb-Version" response
       return $
           if null version || getResponseStatusCode response /= 204
             then Nothing
             else Just . InfluxVersion . T.decodeUtf8 $ head version


getQueryRaw :: Config -> OptionalParams -> Query -> IO A.Value
getQueryRaw config opts query =
    do let url = configServer config `urlAppend` "/query"
           queryString =
             maybe [] credsToQueryString (configCreds config) ++
             optParamsToQueryString opts ++
             [ ("q", Just (T.encodeUtf8 (unQuery query))) ]
       baseReq <- parseUrl url
       let req =
             setRequestMethod "GET" $
             setRequestQueryString queryString baseReq
       res <- httpJSONEither req
       case getResponseBody res of
         Left err -> fail $ "JSON decoding failed: " ++ show err
         Right val -> pure val

-- getQuery :: Config -> Maybe Database -> Query -> IO ???
-- postQueryRaw ::
-- postQuery :: ???
