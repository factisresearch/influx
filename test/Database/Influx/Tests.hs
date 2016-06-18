{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Influx.Tests
   ( htf_thisModulesTests
   ) where

import Database.Influx

import Test.Framework
import Network.HTTP.Client.Conduit
import Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Data.Vector as V

testConfig :: IO Config
testConfig =
    do pure
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
    do config <- testConfig
       res <- ping config
       assertBool $
           maybe False ((>= 1) . T.length . unInfluxVersion) res

test_getQueryRaw :: IO ()
test_getQueryRaw =
    do config <- testConfig
       let pointCount = length . tableValues . head . fromJust . resultTables . head
       res <- pointCount <$> getQueryRaw config (defaultQueryParams { qp_database = Just "_internal" }) (Query "select * from \"httpd\"")
       assertBool $ res >= 1

showDBs :: IO [T.Text]
showDBs =
    do config <- testConfig
       map ((\(String t) -> t) . V.head . influxPointValues) . tableValues . head . fromJust . resultTables . head <$> queryRaw "POST" config defaultQueryParams "SHOW DATABASES"

test_createDropDB :: IO ()
test_createDropDB =
    do config <- testConfig
       postQuery config Nothing "CREATE DATABASE integration_test"
       dbs <- showDBs
       postQuery config Nothing "DROP DATABASE integration_test"
       dbsAfter <- showDBs
       assertBool $ "integration_test" `elem` dbs && "integration_test" `notElem` dbsAfter

test_properLineProtocol :: IO ()
test_properLineProtocol = let line = "cpu,host=server\\ 01,region=uswest value=1i,msg=\"all systems nominal\""
                              repr = InfluxData "cpu" [("host", "server 01"), ("region", "uswest")] [("value", Number 1), ("msg", String "all systems nominal")] Nothing
                              in assertEqual line (serializeInfluxData repr)
