{-# LANGUAGE TemplateHaskell   #-}

module Common.CharacterSheet where

import           Control.Lens          (from, to, (^.))
import           Control.Lens.TH       (makeLenses)
import           Data.Number.Nat1      (Nat1)

data Traits = Traits deriving (Eq, Show)

data EffectSet = EffectSet deriving (Eq, Show)

data CharacterSheet = CharacterSheet
  { _chrSheetTraits      :: Traits
  , _chrSheetEdget       :: EffectSet
  , _chrSheetHinderances :: EffectSet
  , _chrSheetBlessings   :: EffectSet
  , _chrSheetKnacks      :: EffectSet
  , _chrSheetSize        :: Nat1
  , _chrLightArmor       :: Nat1
  } deriving (Show, Eq)
