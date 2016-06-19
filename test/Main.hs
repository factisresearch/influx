{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Framework
import {-@ HTF_TESTS @-} Database.Influx.API.Tests
import {-@ HTF_TESTS @-} Database.Influx.Internal.Helpers.Tests

main :: IO ()
main = htfMain htf_importedTests