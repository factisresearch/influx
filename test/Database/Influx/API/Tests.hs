{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Influx.API.Tests
   ( htf_thisModulesTests
   ) where

import Database.Influx

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
       assertBool $
           "integration_test" `elem` dbs &&
           "integration_test" `notElem` dbsAfter
