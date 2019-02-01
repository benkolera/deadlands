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

import           Control.Monad.Fix       (MonadFix)
import           Data.Foldable           (toList)
import           Data.List.NonEmpty      (NonEmpty, nonEmpty)
import           Data.Map                (Map)
import qualified Data.Map                as Map
--import           Data.Number.Nat         (Nat, toNat)
--import           Data.Number.Nat1        (Nat1, fromNat1)
import           Data.Set                (Set)
import           Reflex.Dom

import           Frontend.Internal       (diffDyn, fget)
--import           Obelisk.Generated.Static
import           Common.CharacterSheet
import           Common.DiceSet
import           Common.Effects

edges
  :: (MonadHold t m, MonadFix m, PostBuild t m, DomBuilder t m)
  => Dynamic t (Set Edges)
  -> m (Event t (NonEmpty Effect))
edges eDyn = elClass "div" "effects-section" $ do
  let eMapDyn = Map.fromList . fmap (\k -> (k,edgeMeta k)) . toList <$> eDyn
  el "h2" $ text "Edges"
  lDyn <- list eMapDyn $ \emDyn -> elClass "div" "effect" $ do
    elClass "span" "name" . dynText $ fget (effectMetaName._Wrapped) emDyn
    pure emDyn
  pure . fmapMaybe nonEmpty . diffDyn calcEffects . joinDynThroughMap $ lDyn

calcEffects :: Eq k => Map k EffectMeta -> Map k EffectMeta -> [Effect]
calcEffects _ _ = (adds <> removes)
  where
    -- Because all of our current edges are passive, we never actually have any
    -- adds or removes. Rip. Let's make the most convoluted 'never' function ever
    -- written for a tick.
    adds :: [Effect]
    adds = []
    removes :: [Effect]
    removes = []
