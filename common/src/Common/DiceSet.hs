module Common.DiceSet where

import Data.Number.Nat1 (Nat1)

data DamageSides = DD4 | DD6 | DD8 | DD10 | DD12 | DD20
data TraitSides  = TD4 | TD6 | TD8 | TD10 | TD12

data Sides = Damage DamageSides | Trait TraitSides

sidesToNat :: Sides -> Nat1
sidesToNat = _

data DiceSet = DiceSet
  { _diceSetSides :: Sides
  , _diceSetNum   :: Nat1
  , _diceSetBonus :: Integer
  }
