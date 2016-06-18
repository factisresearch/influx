{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Influx.Tests
   ( htf_thisModulesTests
   ) where

import Database.Influx

import Network.HTTP.Client.Conduit
import Test.Framework
import qualified Data.Text as T

testConfig :: IO Config
testConfig =
    do manager <- newManager
       pure
           Config
           { configCreds = Just creds
           , configServer = "http://localhost:8086"
           , configManager = manager
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
