{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.HUnit
import Data.Foldable (for_)

import Common.DiceSet
import Common.CharacterSheet

main = defaultMain tests

tests :: TestTree
tests = testGroup "DiceSet"
  [ testCase "stepTrait" $
      for_ [ (DiceSet D4 1 0  , DiceSet D6 1 0 )
           , (DiceSet D6 1 0  , DiceSet D8 1 0 )
           , (DiceSet D6 1 0  , DiceSet D8 1 0 )
           , (DiceSet D8 1 0  , DiceSet D10 1 0)
           , (DiceSet D10 1 0 , DiceSet D12 1 0)
           , (DiceSet D12 1 0 , DiceSet D12 1 2)
           , (DiceSet D12 1 2 , DiceSet D12 1 4)
           , (DiceSet D20 1 0 , DiceSet D12 1 2)
           ] $ \ (i,o) -> stepTrait i @?= o
  , testCase "stepDamage" $
      for_ [ (DiceSet D4 1 0  , DiceSet D6 1 0 )
           , (DiceSet D6 1 0  , DiceSet D8 1 0 )
           , (DiceSet D6 1 0  , DiceSet D8 1 0 )
           , (DiceSet D8 1 0  , DiceSet D10 1 0)
           , (DiceSet D10 1 0 , DiceSet D12 1 0)
           , (DiceSet D12 1 0 , DiceSet D20 1 0)
           , (DiceSet D20 1 0 , DiceSet D20 1 2)
           , (DiceSet D20 1 2 , DiceSet D20 1 4)
           ] $ \ (i,o) -> stepDamage i @?= o
  , testCase "show dice set" $
      for_ [ (DiceSet D4 1 0  , "1d4" )
           , (DiceSet D20 1 2 , "1d20+2")
           , (DiceSet D12 2 0 , "2d12")
           ] $ \ (i, o) -> show i @?= o
  , testCase "show dice set" $
      for_ [ (DiceSet D4 1 0  , "1d4" )
           , (DiceSet D20 1 2 , "1d20+2")
           , (DiceSet D12 2 0 , "2d12")
           ] $ \ (i, o) -> show i @?= o
  , testCase "show dice set" $
      for_ [ ((DiceSet D4 1 0, StandardSet)  , "\\die 1d4" )
           , ((DiceSet D4 1 2, StandardSet)  , "\\die 1d4+2" )
           , ((DiceSet D4 1 0, ExplodingSet)  , "\\die 1d4!" )
           , ((DiceSet D4 1 2, ExplodingSet)  , "\\die 1d4+2!" )
           , ((DiceSet D10 1 0, SetVsTn (TnCheck 7 True))  , "\\die 1d10!kt7s5")
           , ((DiceSet D10 1 2, SetVsTn (TnCheck 7 True))  , "\\die 1d10!kt5s5")
           ] $ \ ((ds, t), o) -> toFgCode t ds @?= o
  ]
