{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}

module Common.CharacterSheet where

import           Control.Lens     ()
import           Control.Lens.TH  (makeLenses, makePrisms)
import           Data.Number.Nat  (Nat)
import           Data.Number.Nat1 (Nat1)
import           Data.Set         (Set)
import qualified Data.Set         as Set
import           Data.Text        (Text)

import           Common.DiceSet

data Concentration a = Concentration
  { _concentrationLevel     :: Nat
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

data DeftnessAptitudes = DeftnessAptitudes
  { _deftnessShootin :: Concentration ShootinConcentrations
  } deriving (Eq, Show)
makeLenses ''DeftnessAptitudes

data NimblenessAptitudes = NimblenessAptitudes
  { _nimblenessFightin    :: Concentration FightinConcentrations
  , _nimblenessClimbin    :: Nat
  , _nimblenessDodge      :: Nat
  , _nimblenessHorseRidin :: Nat
  } deriving (Eq, Show)
makeLenses ''NimblenessAptitudes

data QuicknessAptitudes = QuicknessAptitudes
  { _quicknessQuickLoad :: Concentration QuickLoadConcentrations
  } deriving (Eq, Show)
makeLenses ''QuicknessAptitudes

data StrengthAptitudes = StrengthAptitudes deriving (Eq, Show)
data VigorAptitudes = VigorAptitudes deriving (Eq, Show)

data CognitionAptitudes = CognitionAptitudes
  { _cognitionScruitinize :: Nat
  , _cognitionSearch      :: Nat
  } deriving (Eq, Show)
makeLenses ''CognitionAptitudes

data KnowledgeAptitudes = KnowledgeAptitudes
  { _knowledgeLatin     :: Nat
  , _knowledgeEnglish   :: Nat
  , _knowledgeSpanish   :: Nat
  , _knowledgeOccult    :: Nat
  , _knowledgeTheology  :: Nat
  , _knowledgeChihuahua :: Nat
  } deriving (Eq, Show)
makeLenses ''KnowledgeAptitudes

data MienAptitudes = MienAptitudes
  { _mienOverawe :: Nat
  } deriving (Eq, Show)
makeLenses ''MienAptitudes

data SmartsAptitudes = SmartsAptitudes
  { _smartsStreetwise :: Nat
  } deriving (Eq, Show)
makeLenses ''SmartsAptitudes

data SpiritAptitudes = SpiritAptitudes
  { _spiritGuts  :: Nat
  , _spiritFaith :: Nat
  } deriving (Eq, Show)
makeLenses ''SpiritAptitudes

data Trait a = Trait
  { _traitDiceSet   :: DiceSet
  , _traitAptitudes :: a
  } deriving (Eq, Show)
makeLenses ''Trait

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
  } deriving (Show, Eq)
makeLenses ''CharacterBackground

data CharacterSheet = CharacterSheet
  { _chrSheetTraits      :: Traits
  , _chrSheetEdges       :: Set Edges
  , _chrSheetHinderances :: Set Hinderances
  , _chrSheetBlessings   :: Set Blessings
  , _chrSheetKnacks      :: Set Knacks
  , _chrSheetSize        :: Nat1
  , _chrSheetLightArmor  :: Nat1
  , _chrSheetBackground  :: CharacterBackground
  } deriving (Show, Eq)
makeLenses ''CharacterSheet

gabriela :: CharacterSheet
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
      }
    , _traitsStrength = Trait (DiceSet D6 4 0) $ StrengthAptitudes
    , _traitsVigor = Trait (DiceSet D12 3 0) $ VigorAptitudes
    , _traitsCognition = Trait (DiceSet D12 4 0) $ CognitionAptitudes
      { _cognitionScruitinize = 5
      , _cognitionSearch      = 4
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
    }
  }
