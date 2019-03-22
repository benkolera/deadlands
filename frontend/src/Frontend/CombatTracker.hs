{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module Frontend.CombatTracker where

import           Control.Lens
import           Reflex.Dom

import qualified Data.Dependent.Map    as DMap
import           Data.Monoid           (Endo (Endo))

import           Common.CharacterSheet

-- This is pretty shite.
decActiveBlessingBonuses :: BlessingsMap -> BlessingsMap
decActiveBlessingBonuses = DMap.mapWithKey $ \case
  ArmorOfRighteousness -> fmap (>>= decActiveBonus)
  Smite                -> fmap (>>= decActiveBonus)
  _ -> id
  where
    decActiveBonus (ActiveBonus 1 _) = Nothing
    decActiveBonus (ActiveBonus n v) = Just $ ActiveBonus (n-1) v


combatTracker :: (DomBuilder t m, EventWriter t (Endo CharacterBackground) m) => m ()
combatTracker = do
  buttE <- button "Next Round"
  tellEvent $ (Endo $ chrBgBlessings %~ decActiveBlessingBonuses) <$ buttE
  pure ()
