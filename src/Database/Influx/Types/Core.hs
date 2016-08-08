{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Database.Influx.Types.Core
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
    , QueryFailure(..)
    , QueryResponse(..)
    , WriteFailure(..)
    , WriteResponse(..)
    )
where

import Control.Exception (Exception)
import Data.Aeson ((.:), (.:?))
import Data.String (IsString)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Network.HTTP.Client.Conduit (HttpException, Manager)
import qualified Data.Aeson.Types as A
import qualified Data.Scientific as S
import qualified Data.Vector as V

-- | User credentials.
data Credentials
    = Credentials
    { creds_user :: !Text
    , creds_password :: !Text
    } deriving (Show)

data Config
    = Config
    { config_creds :: !(Maybe Credentials)
    , config_server :: !String
    , config_manager :: !(Maybe Manager)
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
    deriving (Show, Eq)

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
    { point_values :: V.Vector Value
    } deriving (Show, A.FromJSON)

data InfluxTable
    = InfluxTable
    { table_name :: !Text
    , table_columns :: !(V.Vector Text)
    , table_values :: ![InfluxPoint]
    } deriving (Show)

instance A.FromJSON InfluxTable where
    parseJSON =
        A.withObject "InfluxTable" $ \o ->
            do table_name <- o .: "name"
               table_columns <- o .: "columns"
               table_values <- o .: "values"
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
    { result_error :: !(Maybe String)
    , result_tables :: !(Maybe [InfluxTable])
    } deriving (Show)

instance A.FromJSON InfluxResult where
    parseJSON =
        A.withObject "InfluxResult" $ \o ->
            do result_error <- o .:? "error"
               result_tables <- o .:? "series"
               pure InfluxResult {..}

data ParsedTable t
    = ParsedTable
    { parsedRows :: ![t]
    , notParsedRows :: ![InfluxPoint]
    } deriving (Show)

newtype TimeStamp
    = TimeStamp
    { unTimeStamp :: Integer
    } deriving (Show)

data InfluxData
    = InfluxData
    { data_measurement :: !Text
    , data_tags :: ![(Text, Text)]
    , data_fields :: ![(Text, Value)]
    , data_timestamp :: !(Maybe TimeStamp)
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

data QueryFailure
    = BadInfluxQueryRequest !Text -- ^ Unacceptable request (status code 400). Can occur with a syntactically incorrect query. The returned JSON offers further information.
    | QueryFailureHttpException !HttpException -- any other 'HttpException'
    | QueryResponseJsonParseError !String
    deriving (Show, Typeable)

instance Exception QueryFailure

data QueryResponse a
    = QueryResult !a
    | QueryFailed !QueryFailure
    deriving (Show, Functor, Foldable, Traversable)

data WriteFailure
    = BadInfluxWriteRequest !Text -- ^ Unacceptable request (status code 400). Can occur with a Line Protocol syntax error or if a user attempts to write values to a field that previously accepted a different value type. The returned JSON offers further information.
    | InfluxDbDoesNotExist !Text -- ^ Unacceptable request (status code 404). Can occur if a user attempts to write to a database that does not exist. The returned JSON offers further information.
    | InfluxServerError !Text -- ^ Status code 500. The system is overloaded or significantly impaired. Can occur if a user attempts to write to a retention policy that does not exist. The returned JSON offers further information.
    | WriteFailureHttpException !HttpException -- ^ any other 'HttpException'
    deriving (Show, Typeable)

instance Exception WriteFailure

data WriteResponse
    = WriteSuccessful
    | WriteFailed !WriteFailure
    deriving (Show)
