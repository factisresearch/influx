{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Influx.Internal.Helpers.Tests
   ( htf_thisModulesTests
   ) where

import Database.Influx.Internal.Helpers
import Database.Influx.Types
import Test.Framework

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