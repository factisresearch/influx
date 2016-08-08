{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import {-@ HTF_TESTS @-} Database.Influx.API.Tests
import {-@ HTF_TESTS @-} Database.Influx.Internal.Helpers.Tests

import Test.Framework

main :: IO ()
main = htfMain htf_importedTests
