{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database.Influx
import Network.HTTP.Client.Conduit

testConfig :: IO Config
testConfig =
    do manager <- newManager defaultManagerSettings
       pure
          Config
          { configCreds = creds
          , configServer = "localhost:8080"
          , configManager = manager
          }
  where
    creds =
        Credentials
        { credsUser = "root"
        , credsPassword = "root"
        }

main :: IO ()
main = return ()