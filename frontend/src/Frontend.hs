{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Frontend where

import           Control.Lens

import           Clay                     (render)
import           Data.Functor             (void)
import qualified Data.Map                 as Map
import           Data.Monoid.Endo         (Endo (Endo), runEndo)
import           Data.Number.Nat          (Nat, fromNat)
import qualified Data.Text.Lazy           as TL
import           Data.Text.Lens           (packed)
import           Obelisk.Frontend
import           Obelisk.Route
import           Reflex.Dom

import           Common.CharacterSheet
import           Common.DiceSet
import           Common.Route
import           Frontend.Internal        (diffDyn, fget, overEndo)
import           Frontend.Spells          (blessings, edges, hinderances,
                                           knacks)
import           Frontend.Style
import           Frontend.Traits          (traits)
import           Frontend.Wounds          (wounds)
import           Obelisk.Generated.Static

info :: (PostBuild t m, DomBuilder t m) => Dynamic t CharacterBackground -> m ()
info bgDyn = elClass "div" "info" $ do
  el "h2" $dynText (fget chrBgName bgDyn)
  elAttr "img" (Map.fromList [("src",static @"St_Teresa.jpg"), ("width","160")]) blank
  el "dl" $ do
    el "dt" $ text "Occupation"
    el "dd" $ dynText (fget chrBgOccupation bgDyn)
    el "dt" $ text "Hometown"
    el "dd" $ dynText (fget chrBgHomeTown bgDyn)
    el "dt" $ text "Age"
    el "dd" $ dynText (fget (chrBgAge . to show . packed) bgDyn)
    el "dt" $ text "Grit"
    el "dd" $ dynText (fget (chrBgGrit . to show . packed) bgDyn)

woundEffects :: Reflex t => Dynamic t Nat -> Event t (Endo (CharacterStats DiceSet))
woundEffects = diffDyn $ \old new -> Endo $ \c -> applyWounds c (fromNat new - fromNat old)

applyWounds :: CharacterStats DiceSet -> Integer -> CharacterStats DiceSet
applyWounds c wl = c & chrStatsTraits %~ mapTraitsDiceSet (over diceSetBonus (\x -> x - wl))

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Deadlands Character Sheet"
      el "style" . text . TL.toStrict . render $ style
  , _frontend_body = elClass "div" "app" $ mdo
      let initStats = calculateStatsDiceSets (gabriela ^. chrSheetStats)
      let initBg    = gabriela ^. chrSheetBackground
      let applyBgEffects = runEndo initStats . effectsToCharSheet . calculateCharacterBgEffects
      bgDyn    <- foldDyn (flip runEndo) initBg bgDiffE
      statsDyn <- holdDyn (applyBgEffects initBg) (applyBgEffects <$> updated bgDyn)
      bgDiffE <- elClass "div" "character-sheet" $ do
        elClass "div" "traits" $ do
          traits (fget chrStatsTraits statsDyn)
          blank
        healthEffectsE <- elClass "div" "effects" $ do
          info bgDyn
          fmap (overEndo chrBgHealth) <$> wounds
            (fget chrStatsSize statsDyn)
            (maxWind <$> statsDyn)
            (fget chrStatsLightArmor statsDyn)
            (fget chrBgHealth bgDyn)
        spellEffectsEv <- elClass "div" "spells" $ do
          blessChangeEv <- fmap (overEndo chrBgBlessings) <$>
            blessings (fget chrStatsTraits statsDyn) (fget chrBgBlessings bgDyn)
          void $ edges (fget chrBgEdges bgDyn)
          void $ hinderances (fget chrBgHinderances bgDyn)
          void $ knacks (fget chrBgKnacks bgDyn)
          pure $ blessChangeEv
        pure $ healthEffectsE <> spellEffectsEv
      pure ()
  }
