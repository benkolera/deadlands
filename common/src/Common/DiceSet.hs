{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Common.DiceSet where

import Control.Lens ((^.), from, to)
import Control.Lens.TH (makeLenses)
import Data.Bool (bool)
import Data.Foldable (fold)
import Data.Maybe (isNothing)
import Data.Number.Nat (Nat, fromNat)
import Data.Number.Nat1 (Nat1)
import Data.Semigroup ((<>))
import Data.Text (Text)
import Data.Text.Lens (_Text)

data Sides = D4 | D6 | D8 | D10 | D12 | D20 deriving (Eq, Enum)

sidesToNat :: Sides -> Nat1
sidesToNat x = case x of
  D4  -> 4
  D6  -> 6
  D8  -> 8
  D10 -> 10
  D12 -> 12
  D20 -> 20

instance Show Sides where
  show = ("d" <>) . show . sidesToNat

data DiceSet = DiceSet
  { _diceSetSides :: Sides
  , _diceSetNum   :: Nat1
  , _diceSetBonus :: Integer
  } deriving (Eq)
makeLenses ''DiceSet

bonusToStr :: Integer -> String
bonusToStr 0 = ""
bonusToStr n = n ^. to show . to ((if n > 0 then "+" else "") <>)

instance Show DiceSet where
  show (DiceSet d n b) = show n <> show d <> bonusToStr b

stepTrait :: DiceSet -> DiceSet
stepTrait (DiceSet s n b) = case s of
  D12 -> DiceSet s        n (b + 2)
  -- I am dubious that this should be a thing, but lets tighten up the
  -- types later.
  D20 -> DiceSet D12      n (b + 2)
  _   -> DiceSet (succ s) n b

stepDamage :: DiceSet -> DiceSet
stepDamage (DiceSet s n b) = case s of
  D20 -> DiceSet s        n (b + 2)
  _   -> DiceSet (succ s) n b

data TnCheck = TnCheck
  { _tnCheckTn :: Nat
  , _tnCheckSuccesses :: Bool
  } deriving (Eq, Show)

data RollType = StandardSet | ExplodingSet | SetVsTn TnCheck

toFgCode :: RollType -> DiceSet -> Text
toFgCode (SetVsTn (TnCheck tn s)) ds = fold
  -- unfortunately FG needs us to adjust the TN rather than putting a +N on
  -- the dice code so we can't just use show. Which is annoying because it is
  -- different if we have a tn or not. This is really a FG bug but we go
  -- deal with it mostly here.
  [ "\\die "
  , (ds ^. diceSetNum . to show . from _Text)
  , (ds ^. diceSetSides . to show . from _Text)
  , "!"
  , "kt"
  , (max 0 $ fromNat tn - (ds ^. diceSetBonus)) ^. to show . from _Text
  , bool "" "s5" s
  ]
toFgCode StandardSet  ds = ds ^. to show . from _Text . to dieCode
toFgCode ExplodingSet ds = ds ^. to show . from _Text . to dieCode . to (<> "!")

dieCode :: Text -> Text
dieCode = ("\\die " <>)
