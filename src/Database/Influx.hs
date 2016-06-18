{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

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
    , Value
    , InfluxPoint
    , InfluxTable
    , InfluxResult
    , getQueryRaw
    , postQueryRaw
    , FromInfluxValue(..)
    , FromInfluxPoint(..)
    , getQuery
    , InfluxData(..)
    , serializeInfluxData
    ) where

import Control.Arrow (second)
import Control.Monad (void)
import Data.Aeson ((.:), (.:?))
import Data.Either (lefts, rights)
import Data.String (IsString)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Monoid ((<>))
import Data.Text (Text ())
import Network.HTTP.Client.Conduit
import Network.HTTP.Simple
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.ByteString as B
import qualified Data.HVect as HV
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Scientific as S
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

data Value
    = String !Text
    | Number !S.Scientific
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
    -> IO [InfluxResult]
queryRaw method config opts query =
    do let url = configServer config `urlAppend` "/query"
           queryString =
             maybe [] credsToQueryString (configCreds config) ++
             optParamsToQueryString opts ++
             [ ("q", Just (T.encodeUtf8 (unQuery query))) ]
       baseReq <- parseUrl url
       let req =
             setRequestMethod "GET" $
             maybe id setRequestManager (configManager config) $
             setRequestQueryString queryString baseReq
       res <- httpJSONEither req
       case getResponseBody res of
         Left err -> fail $ "JSON decoding failed: " ++ show err
         Right val -> pure (unInfluxResults val)

getQueryRaw :: Config -> OptionalParams -> Query -> IO [InfluxResult]
getQueryRaw = queryRaw "GET"

postQueryRaw :: Config -> OptionalParams -> Query -> IO [InfluxResult]
postQueryRaw = queryRaw "POST"

type Parser = A.Parser

class FromInfluxValue a where
    parseInfluxValue :: Value -> Parser a

instance FromInfluxValue Value where
    parseInfluxValue = pure

instance FromInfluxValue Bool where
    parseInfluxValue val =
        case val of
          Bool b -> pure b
          _ -> fail "expected a bool"

instance FromInfluxValue Text where
    parseInfluxValue val =
        case val of
          String s -> pure s
          _ -> fail "expected a string"

instance FromInfluxValue String where
    parseInfluxValue val =
        case val of
          String s -> pure (T.unpack s)
          _ -> fail "expected a string"

instance FromInfluxValue Integer where
    parseInfluxValue val =
        case val of
          Number s ->
              case S.floatingOrInteger s of
                Left _ -> fail "expected an integer, but got a double"
                Right i -> pure i
          _ -> fail "expected an integer"

instance FromInfluxValue Int where
    parseInfluxValue val =
        case val of
          Number s ->
              case S.toBoundedInteger s of
                Nothing -> fail "expected an int, but got a double or an out-of-range integer"
                Just i -> pure i
          _ -> fail "expected an integer"

instance FromInfluxValue a => FromInfluxValue (Maybe a) where
    parseInfluxValue val =
        case val of
          Null -> pure Nothing
          _ -> Just <$> parseInfluxValue val

{-
instance FromInfluxValue Time.UTCTime where
    parseInfluxValue val =
        case val of
          String s ->
              case Time.parseTimeM True Time.defaultTimeLocale timestampFormat (T.unpack s) of
                Nothing -> fail "could not parse string as timestamp"
                Just time -> pure time
          _ -> fail "expected a time stamp"
        where
          timestampFormat = "%Y-%m-%dT%H:%M:%SZ"
-}

class FromInfluxPoint a where
    parseInfluxPoint :: InfluxPoint -> Parser a

instance FromInfluxPoint InfluxPoint where
    parseInfluxPoint = pure

tupleParser ::
    (FromInfluxValue x, FromInfluxPoint t)
    => InfluxPoint
    -> Parser (x, t)
tupleParser p =
    let v = influxPointValues p
    in if V.length v >= 1
         then
             (,)
                 <$> parseInfluxValue (V.head v)
                 <*> parseInfluxPoint (InfluxPoint (V.tail v))
         else fail "expected a non-empty vector"

instance (FromInfluxValue x, FromInfluxPoint t) => FromInfluxPoint (x, t) where
    parseInfluxPoint = tupleParser

instance FromInfluxPoint (HV.HVect '[]) where
    parseInfluxPoint _ = pure HV.HNil

instance (FromInfluxValue t, FromInfluxPoint (HV.HVect ts)) =>
    FromInfluxPoint (HV.HVect (t ': ts)) where
    parseInfluxPoint p = uncurry (HV.:&:) <$> tupleParser p

data ParsedTable t
    = ParsedTable
    { parsedRows :: [t]
    , pointsThatCouldNotBeParsed :: [InfluxPoint]
    } deriving (Show)

parseInfluxTable ::
  FromInfluxPoint t
  => InfluxTable
  -> ParsedTable t
parseInfluxTable table =
    let parseIfPossible row =
            case A.parseEither parseInfluxPoint row of
              Left _err -> Left row
              Right parsed -> Right parsed
        xs = map parseIfPossible (tableValues table)
        parsedRows = rights xs
        pointsThatCouldNotBeParsed = lefts xs
    in ParsedTable {..}

getQuery ::
    FromInfluxPoint t
    => Config
    -> Maybe DatabaseName
    -> Query
    -> IO (ParsedTable t)
getQuery config mDatabase query =
    do let opts = defaultOptParams { optDatabase = mDatabase }
       results <- getQueryRaw config opts query
       case results of
         [] -> fail "no result"
         _:_:_ -> fail "multiple results"
         [result] ->
             case resultTables result of
               Nothing -> fail "result has no points!"
               Just tables ->
                   case tables of
                     [] -> fail "no tables"
                     _:_:_ -> fail "multiple tables"
                     [table] -> pure (parseInfluxTable table)

-- postQuery :: ???

newtype Timestamp
    = Timestamp
    { unTimestamp :: Integer
    } deriving (Show)

data InfluxData
    = InfluxData
    { dataMeasurement :: Text
    , dataTags :: [(Text, Text)]
    , dataFields :: [(Text, Value)]
    , dataTimestamp :: Maybe Timestamp
    }

serializeValue :: Value -> Maybe Text
serializeValue v =
    case v of
      Number s ->
        case S.floatingOrInteger s :: Either Double Integer of
          Left f ->
            Just $ T.pack $ show f
          Right i ->
            Just $ T.pack (show i) <> "i"
      String s -> Just $ T.pack $ show s
      Bool b ->
        Just $ if b then "true" else "false"
      Null -> Nothing

serializeInfluxData :: InfluxData -> Text
serializeInfluxData d =
    escape (dataMeasurement d) <> " " <>
    T.intercalate "," (map serializeTag (dataTags d)) <> " " <>
    T.intercalate "," (mapMaybe serializeField (dataFields d)) <>
    maybe "" (\t -> " " <> serializeTimeStamp t) (dataTimestamp d)
    where
      serializeTag (k, v) =
          escape k <> "=" <> escape v
      serializeField (k, v) =
          ((escape k <> "=") <>) <$> serializeValue v
      serializeTimeStamp t = T.pack $ show $ unTimestamp t
      escape = T.replace "," "\\," . T.replace " " "\\ "

write :: Config -> OptionalParams -> [InfluxData] -> IO ()
write config opts ds =
    do let url = configServer config `urlAppend` "/write"
           queryString =
             maybe [] credsToQueryString (configCreds config) ++
             optParamsToQueryString opts
           reqBody =
             RequestBodyBS $ T.encodeUtf8 $ T.unlines $
             map serializeInfluxData ds
       baseReq <- parseUrl url
       let req =
             setRequestMethod "POST" $
             maybe id setRequestManager (configManager config) $
             setRequestBody reqBody baseReq
       void $ httpLBS req 
