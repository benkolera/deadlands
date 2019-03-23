{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Common.DiceSet where

import           Control.Lens
import           Control.Lens.TH  (makeLenses, makePrisms)
import           Data.Bool        (bool)
import           Data.Foldable    (fold)
import           Data.Number.Nat1 (Nat1, fromNat1)
import           Data.Semigroup   ((<>))
import           Data.Text        (Text)
import           Data.Text.Lens   (_Text)

data Sides = D4 | D6 | D8 | D10 | D12 | D20 deriving (Eq, Ord, Enum)
makePrisms ''Sides

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
stepTrait = stepHelper D12

stepDamage :: DiceSet -> DiceSet
stepDamage = stepHelper D20

stepHelper :: Sides -> DiceSet -> DiceSet
stepHelper mx ds | ds ^. diceSetSides >= mx =  ds & diceSetSides .~ mx & diceSetBonus %~ (+2)
stepHelper _  ds = ds & diceSetSides %~ succ

data TnCheck = TnCheck
  { _tnCheckTn        :: Nat1
  , _tnCheckSuccesses :: Bool
  } deriving (Eq, Show)
makeLenses ''TnCheck

data RollType = StandardSet | ExplodingSet | SetVsTn TnCheck
makePrisms ''RollType

toFgCode :: RollType -> DiceSet -> Text
toFgCode (SetVsTn (TnCheck tn s)) ds = fold
  -- unfortunately FG needs us to adjust the TN rather than putting a +N on
  -- the dice code so we can't just use show. Which is annoying because it is
  -- different if we have a tn or not. This is really a FG bug but we go
  -- deal with it mostly here.
  [ "/die "
  , (ds ^. diceSetNum . to show . from _Text)
  , (ds ^. diceSetSides . to show . from _Text)
  , "!"
  , "kt"
  , (max 0 $ fromNat1 tn - (ds ^. diceSetBonus)) ^. to show . from _Text
  , bool "" "s5" s
  ]
toFgCode StandardSet  ds = ds ^. to show . from _Text . to dieCode
toFgCode ExplodingSet ds = ds ^. to show . from _Text . to dieCode . to (<> "!")

dieCode :: Text -> Text
dieCode = ("/die " <>)
