{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Database.Influx.API
    ( ping
    , queryRaw
    , getQueryRaw
    , postQueryRaw
    , postQuery
    , FromInfluxValue(..)
    , FromInfluxPoint(..)
    , Cons(..)
    , getQuery
    , serializeInfluxData
    , write
    ) where

import Database.Influx.Types
import Database.Influx.Internal.Helpers
      
import Control.Arrow (second)
import Control.Monad (void)
import Data.Either (lefts, rights)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import Network.HTTP.Client.Conduit
import Network.HTTP.Simple
import qualified Data.Aeson.Types as A
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
    
credsToQueryString :: Credentials -> [(B.ByteString, Maybe B.ByteString)]
credsToQueryString creds =
    fmap (second Just) $
    [ ("u", T.encodeUtf8 (credsUser creds))
    , ("p", T.encodeUtf8 (credsPassword creds))
    ]

epochToBytestring :: EpochPrecision -> B.ByteString
epochToBytestring epoch =
    case epoch of
      Hours -> "h"
      Minutes -> "m"
      Seconds -> "s"
      Milliseconds -> "ms"
      Microseconds -> "us"
      Nanoseconds -> "ns"

queryParamsToQueryString :: QueryParams -> [(B.ByteString, Maybe B.ByteString)]
queryParamsToQueryString opts =
    fmap (second Just) $
    catMaybes
    [ (,) "chunk_size" . T.encodeUtf8 . T.pack . show <$> qp_chunkSize opts
    , (,) "epoch" . epochToBytestring <$> qp_epoch opts
    , (,) "rp" . T.encodeUtf8 <$> qp_retentionPolicy opts
    , (,) "db" . T.encodeUtf8 <$> qp_database opts
    ]


ping :: Config -> IO (Maybe InfluxVersion)
ping config =
    do request <- setRequestMethod "HEAD" <$> parseUrl (urlAppend (configServer config) "/ping")
       response <- httpLBS request
       let version = getResponseHeader "X-Influxdb-Version" response
       return $
           if null version || getResponseStatusCode response /= 204
             then Nothing
             else Just . InfluxVersion . T.decodeUtf8 $ head version

queryRaw ::
       B.ByteString -- ^ HTTP method
    -> Config
    -> QueryParams
    -> Query
    -> IO [InfluxResult]
queryRaw method config opts query =
    do let url = configServer config `urlAppend` "/query"
           queryString =
             maybe [] credsToQueryString (configCreds config) ++
             queryParamsToQueryString opts ++
             [ ("q", Just (T.encodeUtf8 (unQuery query))) ]
       baseReq <- parseUrl url
       let req =
             setRequestMethod method $
             maybe id setRequestManager (configManager config) $
             setRequestQueryString queryString baseReq
       res <- httpJSONEither req
       case getResponseBody res of
         Left err -> fail $ "JSON decoding failed: " ++ show err
         Right val -> pure (unInfluxResults val)

getQueryRaw :: Config -> QueryParams -> Query -> IO [InfluxResult]
getQueryRaw = queryRaw "GET"

postQueryRaw :: Config -> QueryParams -> Query -> IO [InfluxResult]
postQueryRaw = queryRaw "POST"

postQuery :: Config -> Maybe DatabaseName -> Query -> IO ()
postQuery config mDatabase query =
    let params = defaultQueryParams { qp_database = mDatabase }
    in void (postQueryRaw config params query)

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
    do let opts = defaultQueryParams { qp_database = mDatabase }
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

serializeValue :: Value -> Maybe Text
serializeValue v =
    case v of
      Number n -> Just $ T.pack $ show n
      Integer i -> Just $ T.pack (show i) <> "i"
      String s -> Just $ T.pack $ show s
      Bool b ->
        Just $ if b then "true" else "false"
      Null -> Nothing

serializeInfluxData :: InfluxData -> Text
serializeInfluxData d =
    T.intercalate "," (escape (dataMeasurement d) : map serializeTag (dataTags d)) <> " " <>
    T.intercalate "," (mapMaybe serializeField (dataFields d)) <>
    maybe "" (\t -> " " <> serializeTimeStamp t) (dataTimestamp d)
    where
      serializeTag (k, v) =
          escape k <> "=" <> escape v
      serializeField (k, v) =
          ((escape k <> "=") <>) <$> serializeValue v
      serializeTimeStamp t = T.pack $ show $ unTimeStamp t
      escape = T.replace "," "\\," . T.replace " " "\\ "

writeParamsToQueryString :: WriteParams -> [(B.ByteString, Maybe B.ByteString)]
writeParamsToQueryString opts =
    fmap (second Just) $
    catMaybes
    [ (,) "precision" . epochToBytestring <$> wp_precision opts
    , (,) "rp" . T.encodeUtf8 <$> wp_retentionPolicy opts
    ]

write :: Config -> DatabaseName -> WriteParams -> [InfluxData] -> IO ()
write config database opts ds =
    do let url = configServer config `urlAppend` "/write"
           queryString =
             [ ("db", Just (T.encodeUtf8 database)) ] ++
             maybe [] credsToQueryString (configCreds config) ++
             writeParamsToQueryString opts
           reqBody =
             RequestBodyBS $ T.encodeUtf8 $ T.unlines $
             map serializeInfluxData ds
       baseReq <- parseUrl url
       let req =
             setRequestMethod "POST" $
             setQueryString queryString $
             maybe id setRequestManager (configManager config) $
             setRequestBody reqBody baseReq
       void $ httpLBS req