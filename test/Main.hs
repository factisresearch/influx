{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Framework
import {-@ HTF_TESTS @-} Database.Influx.Tests

main :: IO ()
main = htfMain htf_importedTests