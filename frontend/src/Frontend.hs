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
import           Data.Number.Nat          (Nat, fromNat)
import qualified Data.Text.Lazy           as TL
import           Data.Text.Lens           (packed)
import           Obelisk.Frontend
import           Obelisk.Route
import           Reflex.Dom

import           Common.CharacterSheet
import           Common.DiceSet
import           Common.Route
import           Frontend.Internal        (fget)
import           Frontend.Style
import           Frontend.Traits          (traits)
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

applyWounds :: CharacterSheet DiceSet -> Nat -> CharacterSheet DiceSet
applyWounds c wl = c & chrSheetTraits.traitsDeftness.traitAptitudes.deftnessShootin.concentrationLevel.diceSetBonus %~ (\x -> x - fromNat wl)

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Deadlands Character Sheet"
      el "style" . text . TL.toStrict . render $ style
  , _frontend_body = prerender (text "Loading...") $ elClass "div" "app" $ mdo
      let chrDs  = calculateDiceSets gabriela
      -- This forms an infinite recursion because we can't make maxWounds until we have a chrDyn
      -- fix this by making a function woundChanges :: Dynamic t Nat -> Event t (Endo CharacterSheet)
      let chrDyn = applyWounds chrDs <$> maxWoundsE
      --let chrDyn = constDyn (calculateDiceSets gabriela)
      maxWoundsE <- elClass "div" "character-sheet" $ do
        elClass "div" "traits" $ do
          traits (fget chrSheetTraits chrDyn)
          blank
        maxWoundsE <- elClass "div" "effects" $ do
          info (fget chrSheetBackground chrDyn)
          sizeDyn    <- holdUniqDyn (fget chrSheetSize chrDyn)
          vigorSides <- holdUniqDyn (fget (chrSheetTraits.traitsVigor.traitDiceSet.diceSetSides.to sidesToNat.to (*2)) chrDyn)
          lightArmor <- holdUniqDyn (fget chrSheetLightArmor chrDyn)
          wounds (traceDyn "size" sizeDyn) (traceDyn "vigor" vigorSides) (traceDyn "armor" lightArmor)
             (Limbs 0 0 (Just 0) (Just 0) (Just 0) (Just 0))
        elClass "div" "spells" $ do
          el "h1" $ text "Blessings"
          blank
        pure (traceDyn "max wounds" maxWoundsE)
      pure ()
      --display chrDynBad
      display maxWoundsE

  }
