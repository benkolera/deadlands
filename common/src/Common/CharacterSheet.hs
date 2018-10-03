{-# LANGUAGE TemplateHaskell #-}

module Common.CharacterSheet where

import           Control.Lens     (from, to, (^.))
import           Control.Lens.TH  (makeLenses)
import           Data.Number.Nat1 (Nat1)

import           Common.DiceSet   (DiceSet)

data DeftnessAptitudes = DeftnessAptitudes
data NimblenessAptitudes = NimblenessAptitudes
data QuicknessAptitudes = QuicknessAptitudes
data StrengthAptitudes = StrengthAptitudes
data VigorAptitudes = VigorAptitudes
data CognitionAptitudes = CognitionAptitudes
data KnowledgeAptitudes = KnowledgeAptitudes
data MienAptitudes = MienAptitudes
data SmartsAptitudes = SmartsAptitudes
data SpiritAptitudes = SpiritAptitudes

data Trait a = Trait
  { _traitDiceSet   :: DiceSet
  , _traitAptitudes :: a
  }

data Traits = Traits
  { _traitsDeftness   :: Trait DeftnessAptitudes
  , _traitsNimbleness :: Trait NimblenessAptitudes
  , _traitsQuickness  :: Trait QuicknessAptitudes
  , _traitsStrength   :: Trait StrengthAptitudes
  , _traitsVigor      :: Trait VigorAptitudes
  , _traitsCognition  :: Trait CognitionAptitudes
  , _traitsKnowledge  :: Trait KnowledgeAptitudes
  , _traitsMien       :: Trait MienAptitudes
  , _traitsSmarts     :: Trait SmartsAptitudes
  , _traitsSpirit     :: Trait SpiritAptitudes
  } deriving (Eq, Show)

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
