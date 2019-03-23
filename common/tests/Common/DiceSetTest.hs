{-# LANGUAGE OverloadedStrings #-}
module Common.DiceSetTest where

import           Data.Foldable    (for_)
import           Test.Tasty
import           Test.Tasty.HUnit

import           Common.DiceSet

test_stepDiceSide :: [TestTree]
test_stepDiceSide =
  [ testCase "trait capped at 12" $ for_
    [ (DiceSet D4 1 0  , DiceSet D6 1 0 )
    , (DiceSet D6 1 0  , DiceSet D8 1 0 )
    , (DiceSet D6 1 0  , DiceSet D8 1 0 )
    , (DiceSet D8 1 0  , DiceSet D10 1 0)
    , (DiceSet D10 1 0 , DiceSet D12 1 0)
    , (DiceSet D12 1 0 , DiceSet D12 1 2)
    , (DiceSet D12 1 2 , DiceSet D12 1 4)
    , (DiceSet D20 1 0 , DiceSet D12 1 2)
    ] $ \ (i,o) -> stepTrait i @?= o
  , testCase "damage capped at 20" $ for_
    [ (DiceSet D4 1 0  , DiceSet D6 1 0 )
    , (DiceSet D6 1 0  , DiceSet D8 1 0 )
    , (DiceSet D6 1 0  , DiceSet D8 1 0 )
    , (DiceSet D8 1 0  , DiceSet D10 1 0)
    , (DiceSet D10 1 0 , DiceSet D12 1 0)
    , (DiceSet D12 1 0 , DiceSet D20 1 0)
    , (DiceSet D20 1 0 , DiceSet D20 1 2)
    , (DiceSet D20 1 2 , DiceSet D20 1 4)
    ] $ \ (i,o) -> stepDamage i @?= o
  ]

test_showDiceSet :: TestTree
test_showDiceSet = testCase "show dice set" $ for_
  [ (DiceSet D4 1 0  , "1d4" )
  , (DiceSet D20 1 2 , "1d20+2")
  , (DiceSet D12 2 0 , "2d12")
  ] $ \ (i, o) -> show i @?= o

test_toFgCode :: TestTree
test_toFgCode = testCase "toFgCode" $ for_
  [ ((DiceSet D4 1 0, StandardSet)  , "/die 1d4" )
  , ((DiceSet D4 1 2, StandardSet)  , "/die 1d4+2" )
  , ((DiceSet D4 1 0, ExplodingSet)  , "/die 1d4!" )
  , ((DiceSet D4 1 2, ExplodingSet)  , "/die 1d4+2!" )
  , ((DiceSet D10 1 0, SetVsTn (TnCheck 7 True))  , "/die 1d10!kt7s5")
  , ((DiceSet D10 1 2, SetVsTn (TnCheck 7 True))  , "/die 1d10+2!kt7s5")
  ] $ \ ((ds, t), o) -> toFgCode t ds @?= o
