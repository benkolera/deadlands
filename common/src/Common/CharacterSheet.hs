{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

module Common.CharacterSheet where

import           Control.Lens            ()
import           Control.Lens.TH         (makeLenses)
import           Control.Monad           (mfilter)
import           Data.Dependent.Map      (DMap, DSum ((:=>)))
import qualified Data.Dependent.Map      as DMap
import           Data.Dependent.Sum      ((==>))
import           Data.Foldable           (fold)
import           Data.Functor.Identity   (Identity)
import           Data.GADT.Compare       ()
import           Data.GADT.Compare.TH    (deriveGCompare, deriveGEq)
import           Data.GADT.Show          ()
import           Data.GADT.Show.TH       (deriveGShow)
import           Data.List.NonEmpty      (NonEmpty ((:|)))
import           Data.Map                (Map)
import qualified Data.Map                as Map
import           Data.Maybe              (fromMaybe, mapMaybe)
import           Data.Monoid.Endo        (Endo (Endo))
import           Data.Number.Nat         (Nat, fromNat, toNat)
import           Data.Number.Nat1        (Nat1, fromNat1)
import           Data.Semigroup          (Max (Max, getMax))
import           Data.Semigroup.Foldable (foldMap1)
import           Data.Text               (Text)
import qualified Data.Text               as T

import           Control.Lens
import           Data.Number.Nat1        (toNat1)

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
  , _nimblenessSneak      :: l
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

data Edges a where
  ArcaneBlessed :: Edges ()
  Brave         :: Edges ()
  LevelHeaded   :: Edges ()
  NervesOfSteel :: Edges ()
  TheStare      :: Edges ()

deriveGEq ''Edges
deriveGCompare ''Edges
deriveGShow ''Edges

type EdgesMap = DMap Edges Identity

data Hinderances a where
  OathChurch  :: Hinderances ()
  Ferner      :: Hinderances ()
  Poverty     :: Hinderances ()
  Heroic      :: Hinderances ()

deriveGEq ''Hinderances
deriveGCompare ''Hinderances
deriveGShow ''Hinderances

type HinderancesMap = DMap Hinderances Identity

data ActiveBonus = ActiveBonus
  { _activeBonusRoundsLeft :: Nat1
  , _activeBonusValue      :: Nat
  } deriving (Eq, Ord, Show)
makeLenses ''ActiveBonus

data Blessings a where
  ArmorOfRighteousness :: Blessings (Maybe ActiveBonus)
  Smite                :: Blessings (Maybe ActiveBonus)
  Chastise             :: Blessings ()
  RefugeOFaith         :: Blessings ()
  LayOnHands           :: Blessings ()
  HolyRoller           :: Blessings ()
  Protection           :: Blessings ()
  Confession           :: Blessings ()
  MagicResistant       :: Blessings ()

deriveGEq ''Blessings
deriveGCompare ''Blessings
deriveGShow ''Blessings

type BlessingsMap = DMap Blessings Identity

data Knacks a where
  BornOnChristmas :: Knacks ()

deriveGEq ''Knacks
deriveGCompare ''Knacks
deriveGShow ''Knacks

type KnacksMap = DMap Knacks Identity

data LimbDamage = LimbDamage
  { _limbDamageHead     :: Nat
  , _limbDamageTorso    :: Nat
  , _limbDamageLeftArm  :: (Maybe Nat)
  , _limbDamageRightArm :: (Maybe Nat)
  , _limbDamageLeftLeg  :: (Maybe Nat)
  , _limbDamageRightLeg :: (Maybe Nat)
  }
makeLenses ''LimbDamage

maxWounds :: LimbDamage -> Nat
maxWounds ld = getMax . foldMap1 (Max . (ld^.)) $ limbDamageHead :|
  [ limbDamageTorso
  , limbDamageLeftArm.to (fromMaybe 0)
  , limbDamageRightArm.to (fromMaybe 0)
  , limbDamageLeftLeg.to (fromMaybe 0)
  , limbDamageRightLeg.to (fromMaybe 0)
  ]

isDead :: LimbDamage -> Bool
isDead ld = (ld ^. limbDamageHead) > 4 || (ld ^. limbDamageTorso) > 4


data CharacterHealth = CharacterHealth
  { _chrHealthLimbDamage :: LimbDamage
  , _chrHealthWindDamage :: Nat
  }
makeLenses ''CharacterHealth

data CharacterBackground = CharacterBackground
  { _chrBgName        :: Text
  , _chrBgAge         :: Nat1
  , _chrBgOccupation  :: Text
  , _chrBgHomeTown    :: Text
  , _chrBgGrit        :: Nat
  , _chrBgEdges       :: EdgesMap
  , _chrBgHinderances :: HinderancesMap
  , _chrBgBlessings   :: BlessingsMap
  , _chrBgKnacks      :: KnacksMap
  , _chrBgHealth      :: CharacterHealth
  }
makeLenses ''CharacterBackground

newtype CharSize = CharSize { unCharSize :: Nat1 }
makeWrapped ''CharSize

newtype LightArmor = LightArmor { unLightArmor :: Nat }
makeWrapped ''LightArmor

damageToWounds :: CharSize -> LightArmor -> Nat -> Nat
damageToWounds s la d = toNat $
  (max (0::Integer) $ (fromNat d) - (fromNat . unLightArmor $ la))
  `div` (fromNat1 . unCharSize $ s)

data CharacterStats l = CharacterStats
  { _chrStatsTraits     :: Traits l
  , _chrStatsSize       :: CharSize
  , _chrStatsLightArmor :: LightArmor
  }
makeLenses ''CharacterStats

maxWind :: CharacterStats DiceSet -> Nat1
maxWind = view (chrStatsTraits.traitsVigor.traitDiceSet.diceSetSides.to sidesToNat.to (*2))

data CharacterSheet l = CharacterSheet
  { _chrSheetStats      :: CharacterStats Nat  -- Our whole char sheet is the non hydrated one.
  , _chrSheetBackground :: CharacterBackground
  }
makeLenses ''CharacterSheet

aptitudeDice :: DiceSet -> Nat -> DiceSet
aptitudeDice ds 0 = ds & diceSetNum .~ 1 & diceSetBonus %~ (\x -> x - 4)
aptitudeDice ds n = ds & diceSetNum .~ (toNat1 n)

clampModNat :: (Integer -> Integer) -> Nat -> Nat
clampModNat f n = if (intRes < 0) then 0 else toNat intRes
  where
    intRes = f (fromNat n)

calculateStatsDiceSets :: CharacterStats Nat -> CharacterStats DiceSet
calculateStatsDiceSets cs = cs
  & chrStatsTraits %~ calculateTraitsDiceSets
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
  | Bonus (Lens' (CharacterStats DiceSet) Integer) Integer
  | LightArmorBonus Integer
  | DiceSideStep (Lens' (CharacterStats DiceSet) DiceSet) Nat
  | DiceSubstitution
    (Lens' (CharacterStats DiceSet) DiceSet)
    (Lens' (CharacterStats DiceSet) DiceSet)

data EffectMetaMetaValue k
  = PassiveEffect
  | AptitudeCheckEffect Nat1 Nat1 DiceSet (Maybe ActiveBonus) (Maybe ActiveBonus -> Endo (DMap k Identity))

newtype EffectName = EffectName { unEffectName :: T.Text } deriving (Eq, Ord, Show)
makeWrapped ''EffectName

type EffectMap = Map EffectName EffectValue

data EffectMeta k = EffectMeta
  { _effectMetaDesc     :: Text
  , _effectMetaLongDesc :: [Text]
  , _effectMetaMetaVal  :: EffectMetaMetaValue k
  }
makeLenses ''EffectMeta

effectsToCharSheet :: EffectMap -> Endo (CharacterStats DiceSet)
effectsToCharSheet em = foldMap (uncurry applyEffect) $ nonSubs <> subs
  where
    es = Map.toList em
    isSub (DiceSubstitution _ _) = True
    isSub _                      = False
    subs = filter (isSub . snd) es
    nonSubs = filter (not . isSub . snd) es
    applyEffect _ v = case v of
      Special -> Endo id
      BonusAllTraitsAndAptitudes b -> Endo $ chrStatsTraits %~ mapTraitsDiceSet (diceSetBonus %~ (+ b))
      Bonus l b -> Endo $ l %~ (+ b)
      DiceSubstitution dl tl -> Endo $ \cs -> cs & tl .~ (cs ^. dl)
      DiceSideStep l s -> fold $ replicate (fromNat s) (Endo $ l %~ stepTrait)
      LightArmorBonus b -> Endo $ chrStatsLightArmor._Wrapped %~ (\i -> clampModNat (+ b) i)

calculateCharacterBgEffects :: CharacterBackground -> EffectMap
calculateCharacterBgEffects bg = fold
  [ calculateBlessingEffects (bg^.chrBgBlessings)
  , calculateEdgeEffects (bg^.chrBgEdges)
  , calculateHinderanceEffects (bg^.chrBgHinderances)
  , calculateKnackEffects (bg^.chrBgKnacks)
  , calculateWoundEffects (bg^.chrBgHealth.chrHealthLimbDamage)
  , calculateGritEffects (bg^.chrBgGrit)
  ]

en :: Text -> EffectName
en = EffectName

calculateWoundEffects :: LimbDamage -> EffectMap
calculateWoundEffects = maybe Map.empty woundEffect . mfilter (/= 0) . Just . maxWounds
  where
    woundEffect = Map.singleton (en "Wounds") . BonusAllTraitsAndAptitudes . (0-) . fromNat

calculateGritEffects :: Nat -> EffectMap
calculateGritEffects 0 = Map.empty
calculateGritEffects n = Map.singleton
  (en "Grit")
  (Bonus (chrStatsTraits.traitsSpirit.traitAptitudes.spiritGuts.diceSetBonus)
   (fromNat n))

calculateBlessingEffects :: BlessingsMap -> EffectMap
calculateBlessingEffects = Map.fromList . mapMaybe blessingEffect . DMap.toList
  where
    faithSub = DiceSubstitution (chrStatsTraits.traitsSpirit.traitAptitudes.spiritFaith)
    blessingEffect :: DSum Blessings Identity -> Maybe (EffectName,EffectValue)
    blessingEffect (ArmorOfRighteousness :=> Identity bMay) = (\lab -> (en "Armor o' Righteousness", LightArmorBonus $ lab ^. activeBonusValue. to fromNat)) <$> bMay
    blessingEffect (Smite :=> Identity bMay) = (\b -> (en "Smite", DiceSideStep (chrStatsTraits.traitsStrength.traitDiceSet) (b^.activeBonusValue))) <$> bMay
    blessingEffect (Chastise :=> _) = Just $ (en "Chastise", faithSub (chrStatsTraits.traitsMien.traitAptitudes.mienOverawe))
    blessingEffect (RefugeOFaith :=> _) = Just $ (en "Refuge o' Faith", faithSub (chrStatsTraits.traitsNimbleness.traitAptitudes.nimblenessDodge))
    blessingEffect (LayOnHands :=> _) = Just $ (en "Lay on Hands", Special)
    blessingEffect (HolyRoller :=> _) = Just $ (en "Holy Roller", Special)
    blessingEffect (Protection :=> _) = Just $ (en "Protection", Special)
    blessingEffect (Confession :=> _) = Just $ (en "Confession", Special)
    blessingEffect (MagicResistant :=> _) = Just $ (en "Magic Resistant", Special)



calculateEdgeEffects :: EdgesMap -> EffectMap
calculateEdgeEffects = Map.fromList . fmap edgeEffect . DMap.toList
  where
    edgeEffect :: DSum Edges Identity -> (EffectName,EffectValue)
    edgeEffect (ArcaneBlessed :=> _) = (en "Arcane (Blessed)", Special)
    edgeEffect (Brave :=> _) = (en "Brave",Bonus (chrStatsTraits.traitsSpirit.traitAptitudes.spiritGuts.diceSetBonus) 2)
    edgeEffect (LevelHeaded :=> _) = (en "Level Headed", Special)
    edgeEffect (NervesOfSteel :=> _) = (en "Nerves o' Steel", Special)
    edgeEffect (TheStare :=> _) = (en "The Stare", Bonus (chrStatsTraits.traitsMien.traitAptitudes.mienOverawe.diceSetBonus) 2)

calculateHinderanceEffects :: HinderancesMap -> EffectMap
calculateHinderanceEffects = Map.fromList . fmap hinderanceEffect . DMap.toList
  where
    hinderanceEffect :: DSum Hinderances Identity -> (EffectName,EffectValue)
    hinderanceEffect (Heroic :=> _)     = (en "Heroic", Special)
    hinderanceEffect (OathChurch :=> _) = (en "Oath (Church)", Special)
    hinderanceEffect (Ferner :=> _)     = (en "Ferner", Special)
    hinderanceEffect (Poverty :=> _)    = (en "Poverty", Special)

calculateKnackEffects :: KnacksMap -> EffectMap
calculateKnackEffects = Map.fromList . fmap knackEffect . DMap.toList
  where
    knackEffect :: DSum Knacks Identity -> (EffectName,EffectValue)
    knackEffect (BornOnChristmas :=> _) = (en "Born on Christmas", Special)

gabriela :: CharacterSheet Nat
gabriela = CharacterSheet
  { _chrSheetStats = CharacterStats {
      _chrStatsTraits = Traits
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
          , _nimblenessSneak      = 1
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
      , _chrStatsSize = CharSize 6
      , _chrStatsLightArmor = LightArmor 0
      }
  , _chrSheetBackground = CharacterBackground
    { _chrBgName = "Sister Gabriela"
    , _chrBgAge = 42
    , _chrBgOccupation = "Catholic Nun"
    , _chrBgHomeTown = "Paso del Norte, México"
    , _chrBgGrit = 3
    , _chrBgEdges = DMap.fromList
      [ ArcaneBlessed ==> ()
      , Brave ==> ()
      , LevelHeaded ==> ()
      , NervesOfSteel ==> ()
      , TheStare ==> ()
      ]
    , _chrBgHinderances = DMap.fromList
      [ OathChurch ==> ()
      , Ferner ==> ()
      , Poverty ==> ()
      , Heroic ==> ()
      ]
    , _chrBgBlessings = DMap.fromList
      [ ArmorOfRighteousness ==> Nothing
      , Smite ==> Nothing
      , Chastise ==> ()
      , Confession ==> ()
      , LayOnHands ==> ()
      , HolyRoller ==> ()
      , Protection ==> ()
      , MagicResistant ==> ()
      , RefugeOFaith ==> ()
      ]
    , _chrBgKnacks = DMap.fromList
      [BornOnChristmas ==> ()]
    , _chrBgHealth = CharacterHealth
      { _chrHealthLimbDamage = LimbDamage
        { _limbDamageHead     = 0
        , _limbDamageTorso    = 0
        , _limbDamageLeftArm  = Just 0
        , _limbDamageRightArm = Just 0
        , _limbDamageLeftLeg  = Just 0
        , _limbDamageRightLeg = Just 0
        }
      , _chrHealthWindDamage = 0
      }
    }
  }
