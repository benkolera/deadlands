{-# LANGUAGE OverloadedStrings #-}
module Common.StringTest where

import           Test.Tasty
import           Test.Tasty.HUnit

import           Common.Strings   ()

test_nothing :: TestTree
test_nothing = testCase "nothing" $ error "write some tests"
