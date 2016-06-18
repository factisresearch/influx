{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Database.Influx.Types
    ( RetentionPolicy
    , DatabaseName
    , Credentials(..)
    , Config(..)
    , EpochPrecision(..)
    , QueryParams(..)
    , defaultQueryParams
    , InfluxVersion(..)
    , Query(..)
    , Value(..)
    , InfluxPoint(..)
    , InfluxTable(..)
    , InfluxResult(..)
    , InfluxResults(..)
    , ParsedTable(..)
    , TimeStamp(..)
    , InfluxData(..)
    , WriteParams(..)
    , defaultWriteParams
    ) where

import Data.Aeson ((.:), (.:?))
import Data.String (IsString)
import Data.Text (Text)
import Network.HTTP.Client.Conduit (Manager)
import qualified Data.Aeson.Types as A
import qualified Data.Scientific as S
import qualified Data.Vector as V

-- | User credentials
data Credentials
    = Credentials
    { credsUser :: !Text
    , credsPassword :: !Text
    } deriving (Show)

data Config = Config
    { configCreds  :: !(Maybe Credentials)
    , configServer :: !String
    , configManager :: !(Maybe Manager)
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

type DatabaseName = Text

data QueryParams
    = QueryParams
    { qp_chunkSize :: !(Maybe Int)
    , qp_epoch :: !(Maybe EpochPrecision)
    , qp_retentionPolicy :: !(Maybe RetentionPolicy)
    , qp_database :: !(Maybe DatabaseName)
    } deriving (Show)

defaultQueryParams :: QueryParams
defaultQueryParams =
    QueryParams
    { qp_chunkSize = Nothing
    , qp_epoch = Nothing
    , qp_retentionPolicy = Nothing
    , qp_database = Nothing
    }

newtype InfluxVersion
    = InfluxVersion
    { unInfluxVersion :: Text
    } deriving Show

newtype Query
    = Query
    { unQuery :: Text
    } deriving (Show, IsString)

data Value
    = String !Text
    | Number !S.Scientific
    | Integer !Integer -- ^ only used for serialization
    | Bool !Bool
    | Null
    deriving (Show)

instance A.FromJSON Value where
    parseJSON val =
        case val of
          A.String s -> pure (String s)
          A.Number n -> pure (Number n)
          A.Bool b -> pure (Bool b)
          A.Null -> pure Null
          A.Object _ -> fail "didn't expect an object as an influx value"
          A.Array _ -> fail "didn't expect an array as an influx value"

newtype InfluxPoint
    = InfluxPoint
    { influxPointValues :: V.Vector Value
    } deriving (Show, A.FromJSON)

data InfluxTable
    = InfluxTable
    { tableName :: Text
    , tableColumns :: V.Vector Text
    , tableValues :: [InfluxPoint]
    } deriving (Show)

instance A.FromJSON InfluxTable where
    parseJSON =
        A.withObject "InfluxTable" $ \o ->
            do tableName <- o .: "name"
               tableColumns <- o .: "columns"
               tableValues <- o .: "values"
               pure InfluxTable {..}

newtype InfluxResults
    = InfluxResults
    { unInfluxResults :: [InfluxResult]
    } deriving (Show)

instance A.FromJSON InfluxResults where
    parseJSON =
        A.withObject "InfluxResults" $ \o ->
            InfluxResults <$> o .: "results"

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

data ParsedTable t
    = ParsedTable
    { parsedRows :: [t]
    , pointsThatCouldNotBeParsed :: [InfluxPoint]
    } deriving (Show)

newtype TimeStamp
    = TimeStamp
    { unTimeStamp :: Integer
    } deriving (Show)

data InfluxData
    = InfluxData
    { dataMeasurement :: Text
    , dataTags :: [(Text, Text)]
    , dataFields :: [(Text, Value)]
    , dataTimestamp :: Maybe TimeStamp
    }

data WriteParams
    = WriteParams
    { wp_precision :: !(Maybe EpochPrecision)
    , wp_retentionPolicy :: !(Maybe RetentionPolicy)
  }

defaultWriteParams :: WriteParams
defaultWriteParams =
    WriteParams
    { wp_precision = Nothing
    , wp_retentionPolicy = Nothing
    }