{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

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
    , InfluxTable
    , InfluxResult
    , getQueryRaw
    ) where

import Control.Arrow (second)
import Data.Aeson ((.:), (.:?))
import Data.String (IsString)
import Data.Maybe (catMaybes)
import Data.Text (Text ())
import Network.HTTP.Client.Conduit
import Network.HTTP.Simple
import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V

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

data InfluxTable
    = InfluxTable
    { tableName :: Text
    , tableColumns :: V.Vector Text
    , tableValues :: [V.Vector A.Value]
    } deriving (Show)

instance A.FromJSON InfluxTable where
    parseJSON =
        A.withObject "InfluxTable" $ \o ->
            do tableName <- o .: "name"
               tableColumns <- o .: "columns"
               tableValues <- o .: "values"
               pure InfluxTable {..}

data InfluxResult
    = InfluxResult
    { resultError :: Maybe String
    , resultTables :: Maybe [InfluxTable]
    } deriving (Show)

instance A.FromJSON InfluxResult where
    parseJSON =
        A.withObject "InfluxResult" $ \o ->
            do resultError <- o .:? "error"
               resultTables <- o .:? "series"
               pure InfluxResult {..}

newtype InfluxResults
    = InfluxResults
    { unInfluxResults :: [InfluxResult]
    } deriving (Show)

instance A.FromJSON InfluxResults where
    parseJSON =
        A.withObject "InfluxResults" $ \o ->
            InfluxResults <$> o .: "results"

queryRaw ::
       String -- ^ HTTP method
    -> Config
    -> OptionalParams
    -> Query
    -> IO InfluxResults
queryRaw method config opts query =
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

getQueryRaw :: Config -> OptionalParams -> Query -> IO InfluxResults
getQueryRaw = queryRaw "GET"

postQueryRaw :: Config -> OptionalParams -> Query -> IO InfluxResults
postQueryRaw = queryRaw "POST"

-- getQuery :: Config -> Maybe Database -> Query -> IO ???
-- postQueryRaw ::
-- postQuery :: ???
