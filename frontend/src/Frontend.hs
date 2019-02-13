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
import qualified Data.Map                 as Map
import           Data.Monoid.Endo         (Endo(Endo), runEndo)
import           Data.Number.Nat          (Nat, fromNat)
import qualified Data.Text.Lazy           as TL
import           Data.Text.Lens           (packed)
import           Obelisk.Frontend
import           Obelisk.Route
import           Reflex.Dom

import           Common.CharacterSheet
import           Common.DiceSet
import           Common.Route
import           Frontend.Internal        (fget, diffDyn)
import           Frontend.Style
import           Frontend.Traits          (traits)
import           Frontend.Spells          (blessings, edges, hinderances, knacks)
import           Frontend.Wounds          (Limbs (Limbs), wounds)
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


woundEffects :: Reflex t => Dynamic t Nat -> Event t (Endo (CharacterSheet DiceSet))
woundEffects = diffDyn $ \old new -> Endo $ \c -> applyWounds c (fromNat new - fromNat old)

applyWounds :: CharacterSheet DiceSet -> Integer -> CharacterSheet DiceSet
applyWounds c wl = c & chrSheetTraits %~ mapTraitsDiceSet (over diceSetBonus (\x -> x - wl))

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Deadlands Character Sheet"
      el "style" . text . TL.toStrict . render $ style
  , _frontend_body = prerender (text "Loading...") $ elClass "div" "app" $ mdo
      let chrDs       = calculateDiceSets gabriela
      let initEffects = calculateEdgeEffects (chrDs ^. chrSheetEdges)
      let initChrDs   = runEndo chrDs (effectsToCharSheet initEffects)
      chrDyn <- foldDyn (flip runEndo) initChrDs woundsDiffE
      woundsDiffE <- elClass "div" "character-sheet" $ do
        elClass "div" "traits" $ do
          traits (fget chrSheetTraits chrDyn)
          blank
        maxWoundsDyn <- elClass "div" "effects" $ do
          info (fget chrSheetBackground chrDyn)
          wounds
            (fget chrSheetSize chrDyn)
            (fget (chrSheetTraits.traitsVigor.traitDiceSet.diceSetSides.to sidesToNat.to (*2)) chrDyn)
            (fget chrSheetLightArmor chrDyn)
            (Limbs 0 0 (Just 0) (Just 0) (Just 0) (Just 0))
        elClass "div" "spells" $ do
          blessings (fget chrSheetBlessings chrDyn)
          edges (fget chrSheetEdges chrDyn)
          hinderances (fget chrSheetHinderances chrDyn)
          knacks (fget chrSheetKnacks chrDyn)
        pure (woundEffects maxWoundsDyn)
      pure ()
  }
