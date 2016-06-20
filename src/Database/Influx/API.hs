{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Database.Influx.API
    ( ping
    , getQueryRaw
    , getQuery
    , postQueryRaw
    , postQuery
    , write
    ) where

import Database.Influx.Types
import Database.Influx.Internal.Helpers

import Data.Aeson ((.:))
import Control.Exception (catch, toException)
import Control.Monad (void)
import Data.Either (lefts, rights)
import Network.HTTP.Client.Conduit
import Network.HTTP.Simple
import Network.HTTP.Types (Status(..))
import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

ping :: Config -> IO (Maybe InfluxVersion)
ping config =
    do let url = configServer config `urlAppend` "/ping"
       request <- setRequestMethod "HEAD" <$> parseUrl url
       response <- httpLBS request
       let version = getResponseHeader "X-Influxdb-Version" response
       return $
           if null version || getResponseStatusCode response /= 204
             then Nothing
             else Just . InfluxVersion . T.decodeUtf8 $ head version

{-
queryRequestMethod :: Query -> Maybe B.ByteString
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
       B.ByteString -- ^ HTTP method
    -> Config
    -> QueryParams
    -> Query
    -> IO [InfluxResult]
queryRaw method config params query =
    do let url = configServer config `urlAppend` "/query"
           queryString =
             maybe [] credsToQueryString (configCreds config) ++
             queryParamsToQueryString params ++
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
            case parseEither parseInfluxPoint Nothing row of
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
    do let params = defaultQueryParams { qp_database = mDatabase }
       results <- getQueryRaw config params query
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

newtype JsonErrorResponse
    = JsonErrorResponse
    { _unJsonErrorResponse :: T.Text
    } deriving (Show)

instance A.FromJSON JsonErrorResponse where
    parseJSON =
        A.withObject "JsonErrorResponse" $ \o ->
            JsonErrorResponse <$> o .: "error"

write ::
       Config
    -> WriteParams
    -> DatabaseName
    -> [InfluxData]
    -> IO WriteResponse
write config params database ds =
    do let url = configServer config `urlAppend` "/write"
           queryString =
             [ ("db", Just (T.encodeUtf8 database)) ] ++
             maybe [] credsToQueryString (configCreds config) ++
             writeParamsToQueryString params
           reqBody =
             RequestBodyBS $ T.encodeUtf8 $ T.unlines $
             map serializeInfluxData ds
       baseReq <- parseUrl url
       let req =
             (setRequestMethod "POST" $
             setQueryString queryString $
             maybe id setRequestManager (configManager config) $
             setRequestBody reqBody baseReq)
             { checkStatus = checkHttpStatus }
       (httpLBS req >>= handleResponse) `catch` \e ->
           pure $ WriteFailed $ WriteFailureHttpException e
    where
      responseErrorMsg res =
          case A.decode (responseBody res) of
            Just (JsonErrorResponse t) -> t
            Nothing -> T.decodeUtf8 $ LB.toStrict $ responseBody res
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
      expectedErrorStatusCodes = [400, 404, 500]
      checkHttpStatus status@(Status sci _) resHeaders cookieJar =
          if (200 <= sci && sci < 300) || sci `elem` expectedErrorStatusCodes
            then Nothing
            else
                Just $ toException $
                    StatusCodeException status resHeaders cookieJar
