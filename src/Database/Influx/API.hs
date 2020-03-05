{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | This module provides functions for interacting with an InfluxDB database server, i.e.
-- pinging, querying and writing data.
module Database.Influx.API
    ( ping
    , getQueryRaw
    , getQuery
    , postQueryRaw
    , postQuery
    , write
    )
where

import Database.Influx.Types
import Database.Influx.Internal.Helpers

import Data.Aeson ((.:))
import Control.Exception (catch, throw)
import Control.Monad (void)
import Data.Either (lefts, rights)
import Network.HTTP.Client.Conduit
import Network.HTTP.Client.Internal (BodyReader, throwHttp)
import Network.HTTP.Simple hiding (Query)
import Network.HTTP.Types (Status(..))
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Conduit.Attoparsec as C
import qualified Data.Conduit.List as C
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

-- | Pings the InfluxDB server which, in response to that, should send its current API version.
-- This version is returned.
ping :: Config -> IO (Maybe InfluxVersion)
ping config =
    do let url = config_server config `urlAppend` "/ping"
       request <- setRequestMethod "HEAD" <$> parseRequest url
       response <- httpLBS request
       let version = getResponseHeader "X-Influxdb-Version" response
       return $
           if null version || getResponseStatusCode response /= 204
             then Nothing
             else Just . InfluxVersion . T.decodeUtf8 $ head version

{-
queryRequestMethod :: Query -> Maybe BS.ByteString
queryRequestMethod q =
    case (queryStatementType q) of
      "select" ->
           if containsIntoClause q
             then Just "POST"
             else Just "GET"
      "show" -> Just "GET"
      "alter" -> Just "POST"
      "create" -> Just "POST"
      "delete" -> Just "POST"
      "drop" -> Just "POST"
      "kill" -> Just "POST"
      "grant" -> Just "POST"
       _ -> Nothing
    where
      queryStatementType =
          T.toLower . T.takeWhile (/= ' ') . T.strip . unQuery

containsIntoClause :: Query -> Bool
containsIntoClause query =
    if T.isInfixOf "into" q && T.isInfixOf "from" q
      then let tokens = T.words q
               intoIndex = "into" `elemIndex` tokens
               fromIndex = "from" `elemIndex` tokens
               selectIndex = "select" `elemIndex` tokens
          in selectIndex < intoIndex && intoIndex < fromIndex
      else False -- can't contain an INTO-clause
    where q = T.toLower . T.strip $ unQuery query

-}

queryRaw ::
       BS.ByteString -- ^ HTTP method
    -> Config
    -> QueryParams
    -> Query
    -> IO (QueryResponse [InfluxResult])
queryRaw method config params query =
    do let url = config_server config `urlAppend` "/query"
           queryString =
               maybe [] credsToQueryString (config_creds config) ++
               queryParamsToQueryString params ++
               [("q", Just (T.encodeUtf8 (unQuery query)))]
       baseReq <- parseRequest url
       let req =
               setRequestMethod method $
               maybe id setRequestManager (config_manager config) $
               setRequestQueryString queryString baseReq
       httpSink req responseSink `catch` \e ->
           pure $ QueryFailed $ QueryFailureHttpException e
    where
      responseSink res =
          case statusCode (responseStatus res) of
            sci | sci <= 200 && sci < 300 ->
                do parsingRes <- C.sinkParserEither A.json'
                   case parsingRes of
                     Left err -> pure $ QueryFailed $ QueryResponseJsonParseError $ show err
                     Right val ->
                         case A.fromJSON val of
                           A.Error err -> pure $ QueryFailed $ QueryResponseJsonParseError err
                           A.Success (InfluxResults results) -> pure $ QueryResult results
            400 ->
                do bs <- LBS.fromChunks <$> C.consume
                   pure $ QueryFailed $ BadInfluxQueryRequest $ responseErrorMsg (bs <$ res)
            sci -> fail $ "unexpected status code: " ++ show sci

-- | Queries a database using a GET request.
getQueryRaw :: Config -> QueryParams -> Query -> IO (QueryResponse [InfluxResult])
getQueryRaw = queryRaw "GET"

-- | Queries a database using a POST request.
postQueryRaw :: Config -> QueryParams -> Query -> IO (QueryResponse [InfluxResult])
postQueryRaw = queryRaw "POST"

-- | POSTs a query to a specific database (the given database overrides the one
-- provided in the config) without returning the returned query results.
postQuery :: Config -> Maybe DatabaseName -> Query -> IO ()
postQuery config mDatabase query = void (postQueryRaw config params query)
    where params = defaultQueryParams { qp_database = mDatabase }

parseInfluxTable ::
    FromInfluxPoint t
    => InfluxTable
    -> ParsedTable t
parseInfluxTable table =
    let parseIfPossible row =
            case parseEither parseInfluxPoint Nothing row of
              Left _err -> Left row
              Right parsed -> Right parsed
        xs = map parseIfPossible (table_values table)
        parsedRows = rights xs
        notParsedRows = lefts xs
    in ParsedTable {..}

-- | Queries a specified database (the given database overrides the one in the config)
-- with a given query and returns parsed table data.
getQuery ::
    FromInfluxPoint t
    => Config
    -> Maybe DatabaseName
    -> Query
    -> IO (ParsedTable t)
getQuery config mDatabase query =
    do let params = defaultQueryParams { qp_database = mDatabase }
       queryRes <- getQueryRaw config params query
       case queryRes of
         QueryResult results -> parseResults results
         QueryFailed err -> throw err
    where
      parseResults results =
         case results of
           [] -> fail "no result"
           (_:_:_) -> fail "multiple results"
           [result] ->
               case result_tables result of
                 Nothing -> fail "result has no points!"
                 Just tables ->
                     case tables of
                       [] -> fail "no tables"
                       (_:_:_) -> fail "multiple tables"
                       [table] -> pure (parseInfluxTable table)

newtype JsonErrorResponse
    = JsonErrorResponse
    { _unJsonErrorResponse :: T.Text
    } deriving (Show)

instance A.FromJSON JsonErrorResponse where
    parseJSON =
        A.withObject "JsonErrorResponse" $ \o ->
            JsonErrorResponse <$> o .: "error"

-- | Try to parse the response as JSON of the format { "error": "some error string" } to get the
-- error message. If this fails, just return the whole response body.
responseErrorMsg :: Response LBS.ByteString -> T.Text
responseErrorMsg res =
    case A.decode (responseBody res) of
      Just (JsonErrorResponse t) -> t
      Nothing -> T.decodeUtf8 $ LBS.toStrict $ responseBody res

checkResponseWithExpectedErrorCodes ::
       [Int]
    -> Request
    -> Response BodyReader
    -> IO ()
checkResponseWithExpectedErrorCodes expectedErrorCodes _req res =
    let Status sci _ = responseStatus res
    in if 200 <= sci && sci < 300 || sci `elem` expectedErrorCodes
          then return ()
          else do
              chunk <- brReadSome (responseBody res) 1024
              let res' = fmap (const ()) res
              throwHttp $ StatusCodeException res' (LBS.toStrict chunk)

-- | Writes to a specified database (which overrides the one provided in the config) some influx data.
write ::
       Config
    -> WriteParams
    -> DatabaseName
    -> [InfluxData]
    -> IO WriteResponse
write config params database ds =
    do let url = config_server config `urlAppend` "/write"
           queryString =
               [ ("db", Just (T.encodeUtf8 database)) ] ++
               maybe [] credsToQueryString (config_creds config) ++
               writeParamsToQueryString params
           reqBody =
               RequestBodyBS $ T.encodeUtf8 $ T.unlines $
               map serializeInfluxData ds
       baseReq <- parseRequest url
       let req =
               (setRequestMethod "POST" $
               setQueryString queryString $
               maybe id setRequestManager (config_manager config) $
               setRequestBody reqBody baseReq)
               { checkResponse = checkResponseWithExpectedErrorCodes [400, 404, 500] }
       (httpLBS req >>= handleResponse) `catch` \e ->
           pure $ WriteFailed $ WriteFailureHttpException e
    where
      handleResponse res =
          case statusCode (responseStatus res) of
            sci | sci <= 200 && sci < 300 ->
                pure WriteSuccessful
            400 ->
                pure $ WriteFailed $ BadInfluxWriteRequest $ responseErrorMsg res
            404 ->
                pure $ WriteFailed $ InfluxDbDoesNotExist $ responseErrorMsg res
            500 ->
                pure $ WriteFailed $ InfluxServerError $ responseErrorMsg res
            sci -> fail $ "unexpected status code: " ++ show sci
