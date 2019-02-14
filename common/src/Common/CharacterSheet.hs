{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Common.CharacterSheet where

import           Control.Lens     ()
import           Control.Lens.TH  (makeLenses, makePrisms)
import           Data.Foldable    (toList, fold)
import           Data.List        (sortBy)
import           Data.Map         (Map)
import qualified Data.Map         as Map
import           Data.Maybe       (mapMaybe)
import           Data.Monoid.Endo (Endo (Endo))
import           Data.Number.Nat  (Nat, toNat, fromNat)
import           Data.Number.Nat1 (Nat1)
import           Data.Set         (Set)
import qualified Data.Set         as Set
import           Data.String      (IsString)
import           Data.Text        (Text)
import qualified Data.Text        as T

import           Control.Lens
import           Data.Number.Nat1 (toNat1)

import           Common.DiceSet

data Concentration a l = Concentration
  { _concentrationLevel     :: l
  , _concentrationAptitudes :: a
  } deriving (Eq, Show)
makeLenses ''Concentration

instance Functor (Concentration a) where
  fmap = over concentrationLevel

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
  { _deftnessShootin :: Concentration ShootinConcentrations l
  } deriving (Eq, Show, Functor)
makeLenses ''DeftnessAptitudes

data NimblenessAptitudes l = NimblenessAptitudes
  { _nimblenessFightin    :: Concentration FightinConcentrations l
  , _nimblenessClimbin    :: l
  , _nimblenessDodge      :: l
  , _nimblenessHorseRidin :: l
  } deriving (Eq, Show, Functor)
makeLenses ''NimblenessAptitudes

data QuicknessAptitudes l = QuicknessAptitudes
  { _quicknessQuickLoad :: Concentration QuickLoadConcentrations l
  , _quicknessQuickDraw :: Concentration QuickDrawConcentrations l
  } deriving (Eq, Show, Functor)
makeLenses ''QuicknessAptitudes

data StrengthAptitudes l = StrengthAptitudes deriving (Eq, Show, Functor)
data VigorAptitudes l = VigorAptitudes deriving (Eq, Show, Functor)

data CognitionAptitudes l = CognitionAptitudes
  { _cognitionScruitinize :: l
  , _cognitionSearch      :: l
  , _cognitionTrackin     :: l
  } deriving (Eq, Show, Functor)
makeLenses ''CognitionAptitudes

data KnowledgeAptitudes l = KnowledgeAptitudes
  { _knowledgeLatin     :: l
  , _knowledgeEnglish   :: l
  , _knowledgeSpanish   :: l
  , _knowledgeOccult    :: l
  , _knowledgeTheology  :: l
  , _knowledgeChihuahua :: l
  } deriving (Eq, Show, Functor)
makeLenses ''KnowledgeAptitudes

data MienAptitudes l = MienAptitudes
  { _mienOverawe :: l
  } deriving (Eq, Show, Functor)
makeLenses ''MienAptitudes

data SmartsAptitudes l = SmartsAptitudes
  { _smartsStreetwise :: l
  } deriving (Eq, Show, Functor)
makeLenses ''SmartsAptitudes

data SpiritAptitudes l = SpiritAptitudes
  { _spiritGuts  :: l
  , _spiritFaith :: l
  } deriving (Eq, Show, Functor)
makeLenses ''SpiritAptitudes

data Trait a l = Trait
  { _traitDiceSet   :: DiceSet
  , _traitAptitudes :: a l
  } deriving (Eq, Show)
makeLenses ''Trait

mapTraitDiceSet :: Functor a => (DiceSet -> DiceSet) -> Trait a DiceSet -> Trait a DiceSet
mapTraitDiceSet f (Trait ds as) = Trait (f ds) (fmap f as)

data Traits l = Traits
  { _traitsDeftness   :: Trait DeftnessAptitudes l
  , _traitsNimbleness :: Trait NimblenessAptitudes l
  , _traitsQuickness  :: Trait QuicknessAptitudes l
  , _traitsStrength   :: Trait StrengthAptitudes l
  , _traitsVigor      :: Trait VigorAptitudes l
  , _traitsCognition  :: Trait CognitionAptitudes l
  , _traitsKnowledge  :: Trait KnowledgeAptitudes l
  , _traitsMien       :: Trait MienAptitudes l
  , _traitsSmarts     :: Trait SmartsAptitudes l
  , _traitsSpirit     :: Trait SpiritAptitudes l
  } deriving (Eq, Show)
makeLenses ''Traits

-- This can't be a functor because it only makes sense to map on a CharSheet
-- that has been expanded to dicesets. Because there are dicesets in this structure
-- that are independent of the type param, it's best to leave this bit specialised, imo.
mapTraitsDiceSet :: (DiceSet -> DiceSet) -> Traits DiceSet -> Traits DiceSet
mapTraitsDiceSet f old = Traits
    { _traitsDeftness = old ^. traitsDeftness .to (mapTraitDiceSet f)
    , _traitsNimbleness = old ^. traitsNimbleness .to (mapTraitDiceSet f)
    , _traitsQuickness = old ^. traitsQuickness .to (mapTraitDiceSet f)
    , _traitsStrength = old ^. traitsStrength .to (mapTraitDiceSet f)
    , _traitsVigor = old ^. traitsVigor .to (mapTraitDiceSet f)
    , _traitsCognition = old ^. traitsCognition .to (mapTraitDiceSet f)
    , _traitsKnowledge = old ^. traitsKnowledge .to (mapTraitDiceSet f)
    , _traitsMien = old ^. traitsMien .to  (mapTraitDiceSet f)
    , _traitsSmarts = old ^. traitsSmarts .to (mapTraitDiceSet f)
    , _traitsSpirit = old ^. traitsSpirit .to (mapTraitDiceSet f)
    }

data Edges = ArcaneBlessed | Brave | LevelHeaded | NervesOfSteel | TheStare
  deriving (Eq, Ord, Show)
makePrisms ''Edges

data Hinderances = OathChurch | Ferner | Poverty | Heroic
  deriving (Eq, Ord, Show)
makePrisms ''Hinderances

data ActiveBonus = ActiveBonus
  { _activeBonusRoundsLeft :: Nat1
  , _activeBonusValue      :: Nat
  } deriving (Eq, Ord, Show)
makeLenses ''ActiveBonus

data Blessings
  = ArmorOfRighteousness (Maybe ActiveBonus)
  | Smite (Maybe ActiveBonus)
  | Chastise
  | RefugeOFaith
  | LayOnHands
  | HolyRoller
  | Protection
  | Confession
  | MagicResistant
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

clampAddNat :: Nat -> Integer -> Nat
clampAddNat n i = if (intRes < 0) then 0 else toNat intRes
  where
    intRes = (fromNat n) + i

calculateDiceSets :: CharacterSheet Nat -> CharacterSheet DiceSet
calculateDiceSets cs = cs
  & chrSheetTraits %~ calculateTraitsDiceSets
  & chrSheetTraits.traitsSpirit.traitAptitudes.spiritGuts.diceSetBonus %~
    -- Bump up the guts check based on grit
    (+ (cs^.chrSheetBackground.chrBgGrit.to fromNat))
  where
    calculateTraitsDiceSets old = Traits
      { _traitsDeftness = old ^. traitsDeftness . to calculateTraitDiceSets
      , _traitsNimbleness = old ^. traitsNimbleness .to calculateTraitDiceSets
      , _traitsQuickness = old ^. traitsQuickness . to calculateTraitDiceSets
      , _traitsStrength = old ^. traitsStrength. to calculateTraitDiceSets
      , _traitsVigor = old ^. traitsVigor. to calculateTraitDiceSets
      , _traitsCognition = old ^. traitsCognition. to calculateTraitDiceSets
      , _traitsKnowledge = old ^. traitsKnowledge. to calculateTraitDiceSets
      , _traitsMien = old ^. traitsMien. to calculateTraitDiceSets
      , _traitsSmarts = old ^. traitsSmarts. to calculateTraitDiceSets
      , _traitsSpirit = old ^. traitsSpirit. to calculateTraitDiceSets
      }
    calculateTraitDiceSets :: Functor a => Trait a Nat -> Trait a DiceSet
    calculateTraitDiceSets (Trait ds as) = Trait ds (fmap (aptitudeDice ds) as)

data EffectValue
  = Special
  | BonusAllTraitsAndAptitudes Integer
  | Bonus (Lens' (CharacterSheet DiceSet) Integer) Integer
  | LightArmorBonus Integer
  | DiceSideStep (Lens' (CharacterSheet DiceSet) DiceSet) Nat
  | DiceSubstitution
    (Lens' (CharacterSheet DiceSet) DiceSet)
    (Lens' (CharacterSheet DiceSet) DiceSet)

data EffectCreator = NoArgument EffectValue
  | AptitudeCheckSuccesses Nat1 (Lens' (CharacterSheet DiceSet) DiceSet) (Integer -> EffectValue)
  | AptitudeCheckValue Nat1 (Lens' (CharacterSheet DiceSet) DiceSet) (Integer -> EffectValue)

newtype EffectName = EffectName { unEffectName :: T.Text } deriving (Eq, Ord, Show, IsString)
makeWrapped ''EffectName

type EffectMap = Map EffectName EffectValue

data EffectMeta = EffectMeta
  { _effectMetaDesc     :: Text
  , _effectMetaLongDesc :: [Text]
  }
makeLenses ''EffectMeta

effectsToCharSheet :: EffectMap -> Endo (CharacterSheet DiceSet)
effectsToCharSheet em = foldMap (uncurry applyEffect) $ nonSubs <> subs
  where
    es = Map.toList em
    isSub (DiceSubstitution _ _) = True
    isSub _                      = False
    subs = filter (isSub . snd) es
    nonSubs = filter (not . isSub . snd) es
    applyEffect n v = case v of
      Special -> Endo id
      BonusAllTraitsAndAptitudes b -> Endo $ chrSheetTraits %~ mapTraitsDiceSet (diceSetBonus %~ (+ b))
      Bonus l b -> Endo $ l %~ (+ b)
      DiceSubstitution dl tl -> Endo $ \cs -> cs & tl .~ (cs ^. dl)
      DiceSideStep l s -> fold $ replicate (fromNat s) (Endo $ l %~ stepTrait)
      LightArmorBonus b -> Endo $ chrSheetLightArmor %~ (\i -> clampAddNat i b)

-- TODO : This overlaps a lot with the edgesMap in Frontend.Spells . We'll have
-- to refactor this and move it up.
calculateCharacterSheetEffects :: CharacterSheet a -> EffectMap
calculateCharacterSheetEffects cs = fold
  [ calculateBlessingEffects (cs^.chrSheetBlessings)
  , calculateEdgeEffects (cs^.chrSheetEdges)
  , calculateHinderanceEffects (cs^.chrSheetHinderances)
  , calculateKnackEffects (cs^.chrSheetKnacks)
  ]

calculateBlessingEffects :: Set Blessings -> EffectMap
calculateBlessingEffects = Map.fromList . mapMaybe blessingEffect . toList
  where
    faithSub = DiceSubstitution (chrSheetTraits.traitsSpirit.traitAptitudes.spiritFaith)
    blessingEffect (ArmorOfRighteousness bMay) = (\lab -> ("Armor o' Righteousness", LightArmorBonus $ lab ^. activeBonusValue. to fromNat)) <$> bMay
    blessingEffect (Smite bMay) = (\b -> ("Smite", DiceSideStep (chrSheetTraits.traitsStrength.traitDiceSet) (b^.activeBonusValue))) <$> bMay
    blessingEffect Chastise = Just $ ("Chastise", faithSub (chrSheetTraits.traitsMien.traitAptitudes.mienOverawe))
    blessingEffect RefugeOFaith = Just $ ("Refuge o' Faith", faithSub (chrSheetTraits.traitsNimbleness.traitAptitudes.nimblenessDodge))
    blessingEffect LayOnHands = Just $ ("Lay on Hands", Special)
    blessingEffect HolyRoller = Just $ ("Holy Roller", Special)
    blessingEffect Protection = Just $ ("Protection", Special)
    blessingEffect Confession = Just $ ("Confession", Special)
    blessingEffect MagicResistant = Just $ ("Magic Resistant", Special)



calculateEdgeEffects :: Set Edges -> EffectMap
calculateEdgeEffects = Map.fromList . fmap edgeEffect . toList
  where
    edgeEffect ArcaneBlessed = ("Arcane (Blessed)", Special)
    edgeEffect Brave = ("Brave",Bonus (chrSheetTraits.traitsSpirit.traitAptitudes.spiritGuts.diceSetBonus) 2)
    edgeEffect LevelHeaded = ("Level Headed", Special)
    edgeEffect NervesOfSteel = ("Nerves o' Steel", Special)
    edgeEffect TheStare = ("The Stare", Bonus (chrSheetTraits.traitsMien.traitAptitudes.mienOverawe.diceSetBonus) 2)

calculateHinderanceEffects :: Set Hinderances -> EffectMap
calculateHinderanceEffects = Map.fromList . fmap hinderanceEffect . toList
  where
    hinderanceEffect Heroic     = ("Heroic", Special)
    hinderanceEffect OathChurch = ("Oath (Church)", Special)
    hinderanceEffect Ferner     = ("Ferner", Special)
    hinderanceEffect Poverty    = ("Poverty", Special)

calculateKnackEffects :: Set Knacks -> EffectMap
calculateKnackEffects = Map.fromList . fmap knackEffect . toList
  where
    knackEffect BornOnChristmas = ("Born on Christmas", Special)

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
    [ ArmorOfRighteousness (Just $ ActiveBonus 1 9)
    , Smite (Just $ ActiveBonus 1 5)
    , Chastise
    , Confession
    , LayOnHands
    , HolyRoller
    , Protection
    , MagicResistant
    , RefugeOFaith
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
