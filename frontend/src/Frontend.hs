{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE FlexibleContexts  #-}
module Frontend where

import Control.Lens

import           Clay                     (render)
import           Data.Bool                (bool)
import qualified Data.Map                 as Map
import           Data.Number.Nat          (Nat)
import           Data.Number.Nat1         (toNat1)
import qualified Data.Text                as T
import qualified Data.Text.Lazy           as TL
import           Data.Text.Lens           (packed)
import           Obelisk.Frontend
import           Obelisk.Route
import           Reflex.Dom.Core

import           Common.Route
import           Common.CharacterSheet
import           Common.DiceSet
import           Frontend.Style
import           Obelisk.Generated.Static

fget :: Functor f => Getter a b -> f a -> f b
fget g = fmap (view g)

traitName
  :: (PostBuild t m, DomBuilder t m)
  => T.Text -> Dynamic t DiceSet -> m ()
traitName n dsDyn = do
  elClass "span" "trait-name" $ do
    text n
  elClass "span" "trait-dice" $ do
    display dsDyn

concentration
  :: (PostBuild t m, DomBuilder t m)
  => T.Text
  -> Dynamic t (Trait a)
  -> Getter a (Concentration b)
  -> (Dynamic t (Concentration b) -> m ())
  -> m ()
concentration cn tDyn getter childLis = do
  el "li" $ do
    text cn
    elClass "ul" "concentration" $ do
      let cDyn = fget (traitAptitudes.getter) tDyn
      childLis cDyn

aptitude
  :: (PostBuild t m, DomBuilder t m)
  => T.Text
  -> Dynamic t (Trait z)
  -> Dynamic t Nat
  -> m ()
aptitude aName tDyn levelDyn = elClass "li" "pure-aptitude" $ do
  elClass "span" "name" $ text aName
  elClass "span" "value" . display $ aptitudeDice
    <$> (fget traitDiceSet tDyn)
    <*> levelDyn

aptitudeDice :: DiceSet -> Nat -> DiceSet
aptitudeDice ds 0 = ds & diceSetNum .~ 1 & diceSetBonus %~ (\x -> x - 4)
aptitudeDice ds n = ds & diceSetNum .~ (toNat1 n)

concentrationAptitude
  :: (PostBuild t m, DomBuilder t m)
  => T.Text
  -> Dynamic t (Trait z)
  -> Dynamic t (Concentration a)
  -> Getter a Bool
  -> m ()
concentrationAptitude label tDyn cDyn g = do
  let alDyn = (\c ->
                bool
                0
                (c^.concentrationLevel)
                (c^.concentrationAptitudes.g)
             )
        <$> cDyn
  aptitude label tDyn alDyn

pureAptitude
  :: (PostBuild t m, DomBuilder t m)
  => T.Text
  -> Dynamic t (Trait a)
  -> Getter a Nat
  -> m ()
pureAptitude name tDyn getter = aptitude name tDyn (view (traitAptitudes.getter) <$> tDyn)

trait
  :: (PostBuild t m, DomBuilder t m)
  => Dynamic t s
  -> T.Text
  -> Getting (Trait a) s (Trait a)
  -> (Dynamic t (Trait a) -> m ())
  -> m ()
trait traitsDyn tLabel tGetter aptitudes =
  elClass "li" "trait" $ do
    let tDyn = view tGetter <$> traitsDyn
    traitName tLabel (fget traitDiceSet tDyn)
    elClass "ul" "aptitudes" $ aptitudes tDyn

traits ::(PostBuild t m, DomBuilder t m) => Dynamic t Traits -> m ()
traits traitsDyn = elClass "div" "traits" $ do
  el "h1" $ text "Traits"
  el "ul" . trait traitsDyn "Deftness" traitsDeftness $ \tDyn ->
    concentration "Shootin" tDyn deftnessShootin $ \cDyn -> do
      concentrationAptitude "Shotgun" tDyn cDyn shootinShotgun
  el "ul" . trait traitsDyn "Nimbleness" traitsNimbleness $ \tDyn -> do
    concentration "Fightin" tDyn nimblenessFightin $ \cDyn -> do
      concentrationAptitude "Brawlin" tDyn cDyn fightinBrawlin
    pureAptitude "Climbin" tDyn nimblenessClimbin
    pureAptitude "Dodge" tDyn nimblenessDodge


info :: (PostBuild t m, DomBuilder t m) => Dynamic t CharacterBackground -> m ()
info bgDyn = elClass "div" "info" $ do
  el "h2" $dynText (fget chrBgName bgDyn)
  elAttr "img" (Map.fromList [("src",static @"St_Teresa.jpg"), ("width","125")]) blank
  el "dl" $ do
    el "dt" $ text "Occupation"
    el "dd" $ dynText (fget chrBgOccupation bgDyn)
    el "dt" $ text "Hometown"
    el "dd" $ dynText (fget chrBgHomeTown bgDyn)
    el "dt" $ text "Age"
    el "dd" $ dynText (fget (chrBgAge . to show . packed) bgDyn)

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Deadlands Character Sheet"
      el "style" . text . TL.toStrict . render $ style
  , _frontend_body = prerender (text "Loading...") $ elClass "div" "app" $ do
      let chrDyn = constDyn gabriela
      elClass "div" "character-sheet" $ do
        elClass "div" "traits" $ do
          traits (view chrSheetTraits <$> chrDyn)
        elClass "div" "effects" $ do
          info (view chrSheetBackground <$> chrDyn)
        elClass "div" "spells" $ do
          el "h1" $ text "Blessings"
          blank

  }
