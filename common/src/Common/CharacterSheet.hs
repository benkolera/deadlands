{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}

module Common.CharacterSheet where

import           Control.Lens     ()
import           Control.Lens.TH  (makeLenses, makePrisms)
import           Data.Number.Nat  (Nat)
import           Data.Number.Nat1 (Nat1)
import           Data.Set         (Set)
import qualified Data.Set         as Set
import           Data.Text        (Text)

import           Control.Lens
import           Data.Number.Nat1 (toNat1)

import           Common.DiceSet

data Concentration l a = Concentration
  { _concentrationLevel     :: l
  , _concentrationAptitudes :: a
  } deriving (Eq, Show)
makeLenses ''Concentration

data ShootinConcentrations = ShootinConcentrations
  { _shootinShotgun :: Bool
  } deriving (Eq, Show)
makeLenses ''ShootinConcentrations

data FightinConcentrations = FightinConcentrations
  { _fightinBrawlin :: Bool
  } deriving (Eq, Show)
makeLenses ''FightinConcentrations

data QuickLoadConcentrations = QuickLoadConcentrations
  { _quickLoadShotgun :: Bool
  } deriving (Eq, Show)
makeLenses ''QuickLoadConcentrations

data QuickDrawConcentrations = QuickDrawConcentrations
  { _quickDrawShotgun :: Bool
  } deriving (Eq, Show)
makeLenses ''QuickDrawConcentrations

data DeftnessAptitudes l = DeftnessAptitudes
  { _deftnessShootin :: Concentration l ShootinConcentrations
  } deriving (Eq, Show)
makeLenses ''DeftnessAptitudes

data NimblenessAptitudes l = NimblenessAptitudes
  { _nimblenessFightin    :: Concentration l FightinConcentrations
  , _nimblenessClimbin    :: l
  , _nimblenessDodge      :: l
  , _nimblenessHorseRidin :: l
  } deriving (Eq, Show)
makeLenses ''NimblenessAptitudes

data QuicknessAptitudes l = QuicknessAptitudes
  { _quicknessQuickLoad :: Concentration l QuickLoadConcentrations
  , _quicknessQuickDraw :: Concentration l QuickDrawConcentrations
  } deriving (Eq, Show)
makeLenses ''QuicknessAptitudes

data StrengthAptitudes l = StrengthAptitudes deriving (Eq, Show)
data VigorAptitudes l = VigorAptitudes deriving (Eq, Show)

data CognitionAptitudes l = CognitionAptitudes
  { _cognitionScruitinize :: l
  , _cognitionSearch      :: l
  , _cognitionTrackin     :: l
  } deriving (Eq, Show)
makeLenses ''CognitionAptitudes

data KnowledgeAptitudes l = KnowledgeAptitudes
  { _knowledgeLatin     :: l
  , _knowledgeEnglish   :: l
  , _knowledgeSpanish   :: l
  , _knowledgeOccult    :: l
  , _knowledgeTheology  :: l
  , _knowledgeChihuahua :: l
  } deriving (Eq, Show)
makeLenses ''KnowledgeAptitudes

data MienAptitudes l = MienAptitudes
  { _mienOverawe :: l
  } deriving (Eq, Show)
makeLenses ''MienAptitudes

data SmartsAptitudes l = SmartsAptitudes
  { _smartsStreetwise :: l
  } deriving (Eq, Show)
makeLenses ''SmartsAptitudes

data SpiritAptitudes l = SpiritAptitudes
  { _spiritGuts  :: l
  , _spiritFaith :: l
  } deriving (Eq, Show)
makeLenses ''SpiritAptitudes

data Trait l a = Trait
  { _traitDiceSet   :: DiceSet
  , _traitAptitudes :: a l
  } deriving (Eq, Show)
makeLenses ''Trait

data Traits l = Traits
  { _traitsDeftness   :: Trait l DeftnessAptitudes
  , _traitsNimbleness :: Trait l NimblenessAptitudes
  , _traitsQuickness  :: Trait l QuicknessAptitudes
  , _traitsStrength   :: Trait l StrengthAptitudes
  , _traitsVigor      :: Trait l VigorAptitudes
  , _traitsCognition  :: Trait l CognitionAptitudes
  , _traitsKnowledge  :: Trait l KnowledgeAptitudes
  , _traitsMien       :: Trait l MienAptitudes
  , _traitsSmarts     :: Trait l SmartsAptitudes
  , _traitsSpirit     :: Trait l SpiritAptitudes
  } deriving (Eq, Show)
makeLenses ''Traits

data Edges = ArcaneBlessed | Brave | LevelHeaded | NervesOfSteel | TheStare
  deriving (Eq, Ord, Show)
makePrisms ''Edges

data Hinderances = OathChurch | Ferner | Poverty | Heroic
  deriving (Eq, Ord, Show)
makePrisms ''Hinderances

data Blessings
  = ArmorOfRigtheousness
  | Smite
  | Chastise
  | RefugeOFaith
  | LayOnHands
  | HolyRoller
  | Protection
  | Confession
  deriving (Eq, Ord, Show)
makePrisms ''Blessings

data Knacks = BornOnChristmas
  deriving (Eq, Ord, Show)
makePrisms ''Knacks

data CharacterBackground = CharacterBackground
  { _chrBgName       :: Text
  , _chrBgAge        :: Nat1
  , _chrBgOccupation :: Text
  , _chrBgHomeTown   :: Text
  , _chrBgGrit       :: Nat
  } deriving (Show, Eq)
makeLenses ''CharacterBackground

data CharacterSheet l = CharacterSheet
  { _chrSheetTraits      :: Traits l
  , _chrSheetEdges       :: Set Edges
  , _chrSheetHinderances :: Set Hinderances
  , _chrSheetBlessings   :: Set Blessings
  , _chrSheetKnacks      :: Set Knacks
  , _chrSheetSize        :: Nat1
  , _chrSheetLightArmor  :: Nat
  , _chrSheetBackground  :: CharacterBackground
  } deriving (Show, Eq)
makeLenses ''CharacterSheet

aptitudeDice :: DiceSet -> Nat -> DiceSet
aptitudeDice ds 0 = ds & diceSetNum .~ 1 & diceSetBonus %~ (\x -> x - 4)
aptitudeDice ds n = ds & diceSetNum .~ (toNat1 n)

-- TODO: This is shite
calculateDiceSets :: CharacterSheet Nat -> CharacterSheet DiceSet
calculateDiceSets = over chrSheetTraits calculateTraitDiceSets
  where
    calculateTraitDiceSets :: Traits Nat -> Traits DiceSet
    calculateTraitDiceSets old = Traits
      { _traitsDeftness = old ^. traitsDeftness .to calculateDeftnessDiceSets
      , _traitsNimbleness = old ^. traitsNimbleness .to calculateNimblenessDiceSets
      , _traitsQuickness = old ^. traitsQuickness .to calculateQuicknessDiceSets
      , _traitsStrength = old ^. traitsStrength .to calculateStrengthDiceSets
      , _traitsVigor = old ^. traitsVigor .to calculateVigorDiceSets
      , _traitsCognition = old ^. traitsCognition .to calculateCognitionDiceSets
      , _traitsKnowledge = old ^. traitsKnowledge .to calculateKnowledgeDiceSets
      , _traitsMien = old ^. traitsMien .to calculateMienDiceSets
      , _traitsSmarts = old ^. traitsSmarts .to calculateSmartsDiceSets
      , _traitsSpirit = old ^. traitsSpirit .to calculateSpiritDiceSets
      }
    calculateDeftnessDiceSets :: Trait Nat DeftnessAptitudes -> Trait DiceSet DeftnessAptitudes
    calculateDeftnessDiceSets (Trait ds as) = Trait ds $ DeftnessAptitudes
      { _deftnessShootin = concentrationDiceSet deftnessShootin ds as
      }
    calculateNimblenessDiceSets :: Trait Nat NimblenessAptitudes -> Trait DiceSet NimblenessAptitudes
    calculateNimblenessDiceSets (Trait ds as ) = Trait ds $ NimblenessAptitudes
      { _nimblenessFightin    = concentrationDiceSet nimblenessFightin ds as
      , _nimblenessClimbin    = aptitudeDiceSet nimblenessClimbin ds as
      , _nimblenessHorseRidin = aptitudeDiceSet nimblenessHorseRidin ds as
      , _nimblenessDodge      = aptitudeDiceSet nimblenessDodge ds as
      }
    calculateQuicknessDiceSets :: Trait Nat QuicknessAptitudes -> Trait DiceSet QuicknessAptitudes
    calculateQuicknessDiceSets (Trait ds as) = Trait ds $ QuicknessAptitudes
      { _quicknessQuickLoad = concentrationDiceSet quicknessQuickLoad ds as
      , _quicknessQuickDraw = concentrationDiceSet quicknessQuickDraw ds as
      }
    calculateStrengthDiceSets :: Trait Nat StrengthAptitudes -> Trait DiceSet StrengthAptitudes
    calculateStrengthDiceSets (Trait ds _) = Trait ds StrengthAptitudes
    calculateVigorDiceSets :: Trait Nat VigorAptitudes -> Trait DiceSet VigorAptitudes
    calculateVigorDiceSets (Trait ds _) = Trait ds VigorAptitudes
    calculateCognitionDiceSets :: Trait Nat CognitionAptitudes -> Trait DiceSet CognitionAptitudes
    calculateCognitionDiceSets (Trait ds as) = Trait ds $ CognitionAptitudes
      { _cognitionScruitinize = aptitudeDiceSet cognitionScruitinize ds as
      , _cognitionSearch = aptitudeDiceSet cognitionSearch ds as
      , _cognitionTrackin = aptitudeDiceSet cognitionTrackin ds as
      }
    calculateKnowledgeDiceSets :: Trait Nat KnowledgeAptitudes -> Trait DiceSet KnowledgeAptitudes
    calculateKnowledgeDiceSets (Trait ds as) = Trait ds $ KnowledgeAptitudes
      { _knowledgeTheology = aptitudeDiceSet knowledgeTheology ds as
      , _knowledgeLatin = aptitudeDiceSet knowledgeLatin ds as
      , _knowledgeEnglish = aptitudeDiceSet knowledgeEnglish ds as
      , _knowledgeSpanish = aptitudeDiceSet knowledgeSpanish ds as
      , _knowledgeOccult = aptitudeDiceSet knowledgeOccult ds as
      , _knowledgeChihuahua = aptitudeDiceSet knowledgeChihuahua ds as
      }
    calculateMienDiceSets :: Trait Nat MienAptitudes -> Trait DiceSet MienAptitudes
    calculateMienDiceSets (Trait ds as) = Trait ds $ MienAptitudes
      { _mienOverawe = aptitudeDiceSet mienOverawe ds as
      }
    calculateSmartsDiceSets :: Trait Nat SmartsAptitudes -> Trait DiceSet SmartsAptitudes
    calculateSmartsDiceSets (Trait ds as) = Trait ds $ SmartsAptitudes
      { _smartsStreetwise = aptitudeDiceSet smartsStreetwise ds as
      }
    calculateSpiritDiceSets :: Trait Nat SpiritAptitudes -> Trait DiceSet SpiritAptitudes
    calculateSpiritDiceSets (Trait ds as) = Trait ds $ SpiritAptitudes
      { _spiritFaith = aptitudeDiceSet spiritFaith ds as
      , _spiritGuts = aptitudeDiceSet spiritGuts ds as
      }
    concentrationDiceSet :: Getter (a Nat) (Concentration Nat b) -> DiceSet -> a Nat -> Concentration DiceSet b
    concentrationDiceSet g ds as = (as ^. g) & concentrationLevel %~ aptitudeDice ds
    aptitudeDiceSet :: Getter (a Nat) Nat -> DiceSet -> a Nat -> DiceSet
    aptitudeDiceSet g ds = view $ g . to (aptitudeDice ds)

gabriela :: CharacterSheet Nat
gabriela = CharacterSheet
  { _chrSheetTraits = Traits
    { _traitsDeftness = Trait (DiceSet D8 4 0) $ DeftnessAptitudes
      { _deftnessShootin = Concentration 5 $ ShootinConcentrations
        { _shootinShotgun = True
        }
      }
    , _traitsNimbleness = Trait (DiceSet D8 4 0) $ NimblenessAptitudes
      { _nimblenessFightin = Concentration 5 $ FightinConcentrations
        { _fightinBrawlin = True
        }
      , _nimblenessClimbin    = 1
      , _nimblenessHorseRidin = 1
      , _nimblenessDodge      = 0
      }
    , _traitsQuickness = Trait (DiceSet D10 3 0) $ QuicknessAptitudes
      { _quicknessQuickLoad = Concentration 1 $ QuickLoadConcentrations
        { _quickLoadShotgun = True
        }
      , _quicknessQuickDraw = Concentration 1 $ QuickDrawConcentrations
        { _quickDrawShotgun = True
        }
      }
    , _traitsStrength = Trait (DiceSet D6 4 0) $ StrengthAptitudes
    , _traitsVigor = Trait (DiceSet D12 3 0) $ VigorAptitudes
    , _traitsCognition = Trait (DiceSet D12 4 0) $ CognitionAptitudes
      { _cognitionScruitinize = 5
      , _cognitionSearch      = 4
      , _cognitionTrackin     = 3
      }
    , _traitsKnowledge = Trait (DiceSet D8 3 0) $ KnowledgeAptitudes
      { _knowledgeLatin = 2
      , _knowledgeEnglish = 2
      , _knowledgeSpanish = 2
      , _knowledgeOccult = 2
      , _knowledgeTheology = 2
      , _knowledgeChihuahua = 2
      }
    , _traitsMien = Trait (DiceSet D6 4 0) $ MienAptitudes
      { _mienOverawe = 0
      }
    , _traitsSmarts = Trait (DiceSet D8 1 0) $ SmartsAptitudes
      { _smartsStreetwise = 1
      }
    , _traitsSpirit = Trait (DiceSet D12 4 0) $ SpiritAptitudes
      { _spiritFaith = 5
      , _spiritGuts  = 4
      }
    }
  , _chrSheetEdges = Set.fromList
    [ArcaneBlessed, Brave, LevelHeaded, NervesOfSteel, TheStare]
  , _chrSheetHinderances = Set.fromList
    [OathChurch, Ferner, Poverty, Heroic]
  , _chrSheetBlessings = Set.fromList
    [ ArmorOfRigtheousness
    , Smite
    , Chastise
    , Confession
    , LayOnHands
    , HolyRoller
    , Protection
    ]
  , _chrSheetKnacks = Set.fromList
    [BornOnChristmas]
  , _chrSheetSize = 6
  , _chrSheetLightArmor = 0
  , _chrSheetBackground = CharacterBackground
    { _chrBgName = "Sister Gabriela"
    , _chrBgAge = 42
    , _chrBgOccupation = "Catholic Nun"
    , _chrBgHomeTown = "Paso del Norte, México"
    , _chrBgGrit = 3
    }
  }
