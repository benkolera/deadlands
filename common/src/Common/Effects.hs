{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Common.Effects where

import           Control.Lens
import           Control.Lens.TH       (makeLenses, makeWrapped)
import           Data.Monoid.Endo      (Endo(Endo))
import           Data.Number.Nat1      (Nat1)
import           Data.Map              (Map)
import qualified Data.Map              as Map
import           Data.String           (IsString)
import           Data.Text             as T

import           Common.CharacterSheet
import           Common.DiceSet

data EffectValue
  = Special
  | BonusAllTraitsAndAptitudes Integer
  | Bonus (Lens' (CharacterSheet DiceSet) Integer) Integer
  | DiceSubstitution
    (Lens' (CharacterSheet DiceSet) DiceSet)
    (Lens' (CharacterSheet DiceSet) DiceSet)

data EffectCreator = NoArgument EffectValue
  | AptitudeCheckSuccesses Nat1 (Lens' (CharacterSheet DiceSet) DiceSet) (Integer -> EffectValue)
  | AptitudeCheckValue Nat1 (Lens' (CharacterSheet DiceSet) DiceSet) (Integer -> EffectValue)

newtype EffectName = EffectName { unEffectName :: T.Text } deriving (Eq, Ord, Show, IsString)
makeWrapped ''EffectName

-- This is kinda silly, but it is so that we can save it to a backend later
data EffectEvent
  = EffectInsert EffectName EffectValue
  | EffectDelete EffectName

data EffectMeta = EffectMeta
  { _effectMetaName      :: EffectName
  , _effectMetaAbstract  :: T.Text
  , _effectMetaDescLines :: [T.Text]
  , _effectMetaCreator   :: EffectCreator
  }
makeLenses ''EffectMeta

processEffectEvent :: EffectEvent -> Endo (Map EffectName EffectValue)
processEffectEvent e = case e of
  EffectInsert n v  -> Endo $ Map.insert n v
  EffectDelete n -> Endo $ Map.delete n

effectsToCharSheet :: Map EffectName EffectValue -> Endo (CharacterSheet DiceSet)
effectsToCharSheet = foldMap (uncurry applyEffect) . Map.toList
  where
    applyEffect n v = case v of
      Special -> Endo id
      BonusAllTraitsAndAptitudes b -> Endo $ chrSheetTraits %~ mapTraitsDiceSet (diceSetBonus %~ (+ b))
      Bonus l b -> Endo $ l %~ (+ b)
      DiceSubstitution dl tl -> Endo $ \cs -> cs & tl .~ (cs ^. dl)
