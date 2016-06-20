{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Influx.API.Tests
   ( htf_thisModulesTests
   ) where

import Database.Influx

import Data.Monoid ((<>))
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

createDb :: DatabaseName -> IO ()
createDb name =
    let query = Query $ "CREATE DATABASE IF NOT EXISTS " <> name
    in postQuery testConfig Nothing query

dropDb :: DatabaseName -> IO ()
dropDb name =
    let query = Query $ "DROP DATABASE IF EXISTS " <> name
    in postQuery testConfig Nothing query

test_createAndDropDB :: IO ()
test_createAndDropDB =
    do createDb "integration_test"
       dbs <- showDBs
       dropDb "integration_test"
       dbsAfter <- showDBs
       assertBool $
           "integration_test" `elem` dbs &&
           "integration_test" `notElem` dbsAfter

withTestDb :: DatabaseName -> IO () -> IO ()
withTestDb name action =
    do dropDb name
       createDb name
       action
       dropDb name

test_writeWithWrongServerAddress :: IO ()
test_writeWithWrongServerAddress =
    do withTestDb db $
           do res <- write wrongConfig defaultWriteParams db [row]
              let isWriteFailureHttpException =
                      case res of
                        WriteFailed (WriteFailureHttpException _) -> True
                        _ -> False
              assertBool isWriteFailureHttpException
    where
      wrongConfig =
          Config
          { configCreds = Nothing
          , configServer = "https://thisisaserverthatdoesnotexist.co.uk:8086"
          , configManager = Nothing
          }
      db = "foobarblub"
      row = InfluxData "issues" [("source", "client-a")] [("jira-id", Integer 1337)] Nothing

test_writeToNonexistentDb :: IO ()
test_writeToNonexistentDb =
    do res <- write testConfig defaultWriteParams db [row]
       let isDbDoesNotExistError =
               case res of
                 WriteFailed (InfluxDbDoesNotExist _errMsg) -> True
                 _ -> False
       assertBool isDbDoesNotExistError
    where
      db = "thisisadatabasethatdoesnotexist"
      row = InfluxData "issues" [("source", "client-a")] [("jira-id", Integer 1337)] Nothing

test_writeInconsistentData :: IO ()
test_writeInconsistentData =
    withTestDb db $
        do res <- write testConfig defaultWriteParams db rows
           let isBadWriteReqError =
                   case res of
                     WriteFailed (BadInfluxWriteRequest _) -> True
                     _ -> False
           assertBool isBadWriteReqError
    where
      db = "foobarblub"
      rows =
          -- writing this fails because the types for "jira-id" don't match
          [ InfluxData "issues" [("source", "client-a")] [("jira-id", Integer 1337)] Nothing
          , InfluxData "issues" [("source", "client-a")] [("jira-id", String "1337")] Nothing
          ]
