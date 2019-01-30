{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Frontend.Spells where

import           Control.Lens

import           Control.Lens.TH         (makeLenses)
import           Control.Monad.Fix       (MonadFix)
import           Data.Foldable           (toList)
import           Data.Functor            (void)
import           Data.List.NonEmpty      (NonEmpty ((:|)), nonEmpty)
import           Data.Map                (Map)
import qualified Data.Map                as Map
--import           Data.Number.Nat         (Nat, toNat)
--import           Data.Number.Nat1        (Nat1, fromNat1)
import           Data.Set                (Set)
import qualified Data.Text               as T
import           Reflex.Dom

import           Frontend.Internal       (diffDyn, fget)
--import           Obelisk.Generated.Static
import           Common.CharacterSheet
import           Common.DiceSet

data EffectType = Special | Bonus (Lens' (CharacterSheet DiceSet) Integer) Integer

-- This will need a modify and a probably a refactor when this happens!!
data EffectAction = EffectAdd | EffectRemove
data Effect = Effect EffectAction EffectType

data EffectMeta = EffectMeta
  { _effectMetaName      :: T.Text
  , _effectMetaAbstract  :: T.Text
  , _effectMetaDescLines :: [T.Text]
  , _effectMetaType      :: EffectType
  }
makeLenses ''EffectMeta

edgeMeta :: Edges -> EffectMeta
edgeMeta e = case e of
  ArcaneBlessed -> EffectMeta "Arcane:Blessed" "The ablility to cast Blessed spells" [] Special
  Brave         -> EffectMeta "Brave" "" [] (Bonus (chrSheetTraits.traitsSpirit.traitAptitudes.spiritGuts.diceSetBonus) 2)
  LevelHeaded   -> EffectMeta "Level Headed" "" [] Special
  NervesOfSteel -> EffectMeta "Nerves of Steel" "" [] Special
  TheStare      -> EffectMeta "The Stare" "" [] (Bonus (chrSheetTraits.traitsMien.traitAptitudes.mienOverawe.diceSetBonus) 2)

edges
  :: (MonadHold t m, MonadFix m, PostBuild t m, DomBuilder t m, DomBuilderSpace m ~ GhcjsDomSpace)
  => Dynamic t (Set Edges)
  -> m (Event t (NonEmpty Effect))
edges eDyn = elClass "div" "effects-section" $ do
  let eMapDyn = Map.fromList . fmap (\k -> (k,edgeMeta k)) . toList <$> eDyn
  el "h2" $ text "Edges"
  lDyn <- list eMapDyn $ \emDyn -> elClass "div" "effect" $ do
    elClass "span" "name" . dynText $ fget effectMetaName emDyn
    pure emDyn
  pure . fmapMaybe nonEmpty . diffDyn calcEffects . joinDynThroughMap $ lDyn

calcEffects :: Eq k => Map k EffectMeta -> Map k EffectMeta -> [Effect]
calcEffects old new = (adds <> removes)
  where
    -- Because all of our current edges are passive, we never actually have any
    -- adds or removes. Rip. Let's make the most convoluted 'never' function ever
    -- written for a tick.
    adds :: [Effect]
    adds = []
    removes :: [Effect]
    removes = []
