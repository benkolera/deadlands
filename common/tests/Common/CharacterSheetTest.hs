{-# LANGUAGE OverloadedStrings #-}
module Common.CharacterSheetTest where

import           Test.Tasty
import           Test.Tasty.HUnit

import           Common.CharacterSheet

test_nothing :: TestTree
test_nothing = testCase "nothing" $ error "write some tests"
