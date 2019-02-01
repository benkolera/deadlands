{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Common.Effects where

import           Control.Lens
import           Control.Lens.TH       (makeLenses, makeWrapped)
import           Data.String           (IsString)
import           Data.Text             as T

import           Common.CharacterSheet
import           Common.DiceSet

data EffectType
  = Special
  | BonusAllTraitsAndAptitudes Integer
  | Bonus  (Lens' (CharacterSheet DiceSet) Integer)Integer
  | DiceSubstitution
    (Lens' (CharacterSheet DiceSet) DiceSet)
    (Lens' (CharacterSheet DiceSet) DiceSet)

newtype EffectName = EffectName { unEffectName :: T.Text } deriving (Eq, Ord, Show, IsString)
makeWrapped ''EffectName

-- This will need a modify and a probably a refactor when this happens!!
data EffectAction = EffectAdd | EffectRemove
data Effect = Effect EffectName EffectAction EffectType

data EffectMeta = EffectMeta
  { _effectMetaName      :: EffectName
  , _effectMetaAbstract  :: T.Text
  , _effectMetaDescLines :: [T.Text]
  , _effectMetaType      :: EffectType
  }
makeLenses ''EffectMeta
