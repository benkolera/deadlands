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
import           Data.Bool               (bool)
import           Data.Functor            (void)
import           Data.Foldable           (toList)
import           Data.List.NonEmpty      (NonEmpty ((:|)))
import qualified Data.Map                   as Map
import           Data.Maybe              (fromMaybe, maybe)
import           Data.Number.Nat         (Nat, toNat)
import           Data.Number.Nat1        (Nat1, fromNat1)
import           Data.Semigroup          (Max (Max, getMax))
import           Data.Semigroup.Foldable (foldMap1)
import           Data.Set                (Set)
import qualified Data.Text               as T
import           Reflex.Dom

import           Frontend.Internal          (fget)
--import           Obelisk.Generated.Static
import           Common.CharacterSheet

data EffectMeta = EffectMeta
  { _effectMetaName :: T.Text
  , _effectMetaAbstract :: T.Text
  , _effectMetaDescLines :: [T.Text]
  }
makeLenses ''EffectMeta

edgeMeta :: Edges -> EffectMeta
edgeMeta e = case e of
  ArcaneBlessed -> EffectMeta "Arcane:Blessed" "The ablility to cast Blessed spells" []
  Brave         -> EffectMeta "Brave" "" []
  LevelHeaded   -> EffectMeta "Level Headed" "" []
  NervesOfSteel -> EffectMeta "Nerves of Steel" "" []
  TheStare      -> EffectMeta "The Stare" "" []

edges
  :: (MonadHold t m, MonadFix m, PostBuild t m, DomBuilder t m, DomBuilderSpace m ~ GhcjsDomSpace)
  => Dynamic t (Set Edges)
  -> m ()
edges eDyn = elClass "div" "effects-section" $ do
  let eMapDyn = Map.fromList . fmap (\k -> (k,edgeMeta k)) . toList <$> eDyn
  el "h2" $ text "Edges"
  void . list eMapDyn $ \emDyn -> elClass "div" "effect" $ do
    elClass "span" "name" . dynText $ fget effectMetaName emDyn
