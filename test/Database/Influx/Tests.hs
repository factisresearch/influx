{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Influx.Tests
   ( htf_thisModulesTests
   ) where

import Database.Influx

import Data.Maybe (fromJust)
import Test.Framework
import qualified Data.Text as T

testConfig :: Config
testConfig =
    Config
    { configCreds = Just creds
    , configServer = "http://localhost:8086"
    , configManager = Nothing
    }
    where
      creds =
          Credentials
          { credsUser = "root"
          , credsPassword = "root"
          }

test_ping :: IO ()
test_ping =
    do res <- ping testConfig
       assertBool $
           maybe False ((>= 1) . T.length . unInfluxVersion) res

test_getQueryRaw :: IO ()
test_getQueryRaw =
    do let pointCount = length . tableValues . head . fromJust . resultTables . head
       res <- pointCount <$> getQueryRaw testConfig (defaultQueryParams { qp_database = Just "_internal" }) (Query "select * from \"httpd\"")
       assertBool $ res >= 1

showDBs :: IO [T.Text]
showDBs =
    do queryRes <- getQuery testConfig Nothing "SHOW DATABASES"
       pure $ map (\(Cons s ()) -> s) (parsedRows queryRes)

test_createDropDB :: IO ()
test_createDropDB =
    do postQuery testConfig Nothing "CREATE DATABASE integration_test"
       dbs <- showDBs
       postQuery testConfig Nothing "DROP DATABASE integration_test"
       dbsAfter <- showDBs
       assertBool $ "integration_test" `elem` dbs && "integration_test" `notElem` dbsAfter

test_properLineProtocol :: IO ()
test_properLineProtocol =
    let line = "cpu,host=server\\ 01,region=uswest value=1i,msg=\"all systems nominal\""
        repr =
            InfluxData
            { dataMeasurement = "cpu"
            , dataTags = [("host", "server 01"), ("region", "uswest")]
            , dataFields = [("value", Integer 1), ("msg", String "all systems nominal")]
            , dataTimestamp = Nothing
            }
    in assertEqual line (serializeInfluxData repr)
