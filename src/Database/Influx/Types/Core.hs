{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | This module provides all core data types that represent entities needed
-- for communication with an InfluxDB database, e.g. user credentials and configurations,
-- database item ("point") types and write/query request results.
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

-- | A config that will be used for every interaction with the database.
data Config
    = Config
    { config_creds :: !(Maybe Credentials)
    -- ^ The credentials that are used when an authentification at the server
    -- is needed.
    , config_server :: !String
    -- ^ A server address (host name).
    , config_manager :: !(Maybe Manager)
    -- ^ TODO
    }

-- | A retention policy (RP) is the part of InfluxDB’s data structure that
-- describes for how long InfluxDB keeps data (duration) and how many copies of
-- those data are stored in the cluster (replication factor). A database can have
-- several RPs and RPs are unique per database. Once a database is created,
-- an RP called @default@ will be initialized. It has infinite retention duration and
-- a replication factor equal to 1.
type RetentionPolicy = Text

-- | An epoch precision defines the precision/unit of the data timestamps.
data EpochPrecision
    = Hours
    | Minutes
    | Seconds
    | Milliseconds
    | Microseconds
    | Nanoseconds
    deriving (Eq, Show)

-- | For interaction with an InfluxDB database, a specific identifying database name is
-- needed. This is what this type is for.
type DatabaseName = Text

-- | Parameters that specify how much and what should be queried.
data QueryParams
    = QueryParams
    { qp_chunkSize :: !(Maybe Int)
    -- ^ The number of 'InfluxPoint's returned per batch.
    -- By default, InfluxDB returns points in batches of 10,000 points.
    , qp_epoch :: !(Maybe EpochPrecision)
    -- ^ Makes the query return epoch timestamps with the specified precision.
    -- By default, InfluxDB returns timestamps in RFC3339 format with nanosecond precision.
    , qp_retentionPolicy :: !(Maybe RetentionPolicy)
    -- ^ The target retention policy for the query.
    -- InfluxDB queries the DEFAULT retention policy if you do not specify a retention policy.
    , qp_database :: !(Maybe DatabaseName)
    -- ^ The target database name for the query.
    } deriving (Show)

-- | Default query parameters with no parameters initialized to a specific value.
--
--     * @qp_chunkSize@ is 10,000.
--     * @qp_epoch@ is TODO.
--     * @qp_retentionPolicy@ is @"default"@.
--     * @qp_database@ is the empty string.
defaultQueryParams :: QueryParams
defaultQueryParams =
    QueryParams
    { qp_chunkSize = Nothing
    , qp_epoch = Nothing
    , qp_retentionPolicy = Nothing
    , qp_database = Nothing
    }

-- | A string specifying the version of InfluxDB.
newtype InfluxVersion
    = InfluxVersion
    { unInfluxVersion :: Text
    } deriving (Show)

-- | An SQL query to the database.
newtype Query
    = Query
    { unQuery :: Text
    } deriving (Show, IsString)

-- | An abstraction over the different types of value that can be stored in an InfluxDB database.
data Value
    = String !Text
    | Number !S.Scientific
    -- ^ This is the normal representation of a numeric type inside a database.
    | Integer !Integer
    -- ^ /Note/: This is only used for serialization.
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

-- | A linear sequence of InfluxDB values.
newtype InfluxPoint
    = InfluxPoint
    { point_values :: V.Vector Value
    } deriving (Show, A.FromJSON)

-- | A database table equipped with a name, column titles and a two-dimensional
-- collection of InfluxDB values.
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

-- | A list of low level results of querying a database, basically
-- an interim representation before 'QueryResult'.
newtype InfluxResults
    = InfluxResults
    { unInfluxResults :: [InfluxResult]
    } deriving (Show)

instance A.FromJSON InfluxResults where
    parseJSON =
        A.withObject "InfluxResults" $ \o ->
            InfluxResults <$> o .: "results"

-- | A low level representation of a query result, only returned from 'queryRaw'.
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

-- | Result of parsing an 'InfluxTable'. Comprises lists of successfully parsed rows
-- and rows that could not be parsed.
data ParsedTable t
    = ParsedTable
    { parsedRows :: ![t]
    , notParsedRows :: ![InfluxPoint]
    } deriving (Show)

-- | A time stamp expressed in the number of nanoseconds since the epoch (1970-01-01).
newtype TimeStamp
    = TimeStamp
    { unTimeStamp :: Integer
    } deriving (Show)

-- | InfluxDB's data structure: a measurement name, metadata and the actual collected data. Also
-- equipped with a timestamp.
data InfluxData
    = InfluxData
    { data_measurement :: !Text
    -- ^ The part of InfluxDB’s structure that describes the data stored in the associated fields.
    , data_tags :: ![(Text, Text)]
    -- ^ The key-value pair in InfluxDB’s data structure that records metadata. Tags are
    -- an optional part of InfluxDB’s data structure but they are useful for storing
    -- commonly-queried metadata; tags are indexed so queries on tags are performant.
    -- /Query tip/: Compare tags to fields; fields are not indexed.
    , data_fields :: ![(Text, Value)]
    -- ^ The key-value pair in InfluxDB’s data structure that records metadata and the
    -- actual data value. Fields are required in InfluxDB’s data structure and they
    -- are not indexed - queries on field values scan all points that match the
    -- specified time range and, as a result, are not performant relative to tags.
    -- /Query tip/: Compare fields to tags; tags are indexed.
    , data_timestamp :: !(Maybe TimeStamp)
    -- ^ The date and time associated with a point. All time in InfluxDB is UTC.
    }

-- | Parameters that specify the RP and time stamp precision of a 'write' action.
data WriteParams
    = WriteParams
    { wp_precision :: !(Maybe EpochPrecision)
    -- ^ The precision of the supplied Unix time values.
    -- If not present timestamps are assumed to be in nanoseconds.
    , wp_retentionPolicy :: !(Maybe RetentionPolicy)
    -- ^ The target retention policy for the write.
    -- If not present the default retention policy is used.
    }

-- | Write with the timestamps in nanoseconds and the default retention policy.
defaultWriteParams :: WriteParams
defaultWriteParams =
    WriteParams
    { wp_precision = Nothing
    , wp_retentionPolicy = Nothing
    }

-- | The result of a failed 'postQuery' or 'getQuery' request.
data QueryFailure
    = BadInfluxQueryRequest !Text
    -- ^ Unacceptable request (status code 400). Can occur with a syntactically
    -- incorrect query. The returned JSON offers further information.
    | QueryFailureHttpException !HttpException -- ^ any other 'HttpException'
    | QueryResponseJsonParseError !String
    deriving (Show, Typeable)

instance Exception QueryFailure

-- | The result of a 'postQuery' or 'getQuery' (TODO) action: Either
-- a polymorphic result or a 'QueryFailure'.
data QueryResponse a
    = QueryResult !a
    | QueryFailed !QueryFailure
    deriving (Show, Functor, Foldable, Traversable)

-- | Result of a failed 'write' request.
data WriteFailure
    = BadInfluxWriteRequest !Text
    -- ^ Unacceptable request (status code 400). Can occur with a Line Protocol syntax error
    -- or if a user attempts to write values to a field that previously accepted
    -- a different value type. The returned JSON offers further information.
    | InfluxDbDoesNotExist !Text
    -- ^ Unacceptable request (status code 404). Can occur if a user attempts
    -- to write to a database that does not exist.
    -- The returned JSON offers further information.
    | InfluxServerError !Text
    -- ^ Status code 500. The system is overloaded or significantly impaired.
    -- Can occur if a user attempts to write to a retention policy that does not exist.
    -- The returned JSON offers further information.
    | WriteFailureHttpException !HttpException
    -- ^ any other 'HttpException'
    deriving (Show, Typeable)

instance Exception WriteFailure

-- | The result of a 'write' action: Either success or a failure parametrized over
-- an the error type 'WriteFailure'.
data WriteResponse
    = WriteSuccessful
    | WriteFailed !WriteFailure
    deriving (Show)
