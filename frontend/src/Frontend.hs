{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE FlexibleContexts  #-}
module Frontend where

import Control.Lens

import           Clay                     (render)
import           Data.Bool                (bool)
import           Data.Foldable            (traverse_)
import           Data.Functor             (void)
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
  -> m ()
  -> m ()
concentration cn childLis = do
  el "li" $ do
    text cn
    elClass "ul" "concentration" $ do
      childLis

aptitude
  :: (PostBuild t m, DomBuilder t m)
  => T.Text
  -> Dynamic t DiceSet
  -> m ()
aptitude aName dsDyn = elClass "li" "pure-aptitude" $ do
  elClass "span" "name" $ text aName
  elClass "span" "value" $ display dsDyn

aptitudeDice :: DiceSet -> Nat -> DiceSet
aptitudeDice ds 0 = ds & diceSetNum .~ 1 & diceSetBonus %~ (\x -> x - 4)
aptitudeDice ds n = ds & diceSetNum .~ (toNat1 n)

concentrationExtract
  :: Applicative (Dynamic t)
  => Dynamic t DiceSet
  -> Dynamic t (Concentration a)
  -> Getter a Bool
  -> Dynamic t (Maybe DiceSet)
concentrationExtract traitDsDyn cDyn g =
  (\ds c -> bool Nothing (Just (aptitudeDice ds (c ^. concentrationLevel))) (view (concentrationAptitudes.g) c))
  <$> traitDsDyn
  <*> cDyn

trait
  :: (PostBuild t m, DomBuilder t m)
  => Dynamic t s
  -> T.Text
  -> Getting (Trait a) s (Trait a)
  -> (Dynamic t (Trait a) -> Dynamic t DiceSet -> m ())
  -> m ()
trait traitsDyn tLabel tGetter aptitudes =
  elClass "li" "trait" $ do
    let tDyn = view tGetter <$> traitsDyn
    let tDsDyn = view traitDiceSet <$> tDyn
    traitName tLabel tDsDyn
    elClass "ul" "aptitudes" $ aptitudes tDyn tDsDyn

traits ::(PostBuild t m, DomBuilder t m) => Dynamic t Traits -> m ()
traits traitsDyn = elClass "div" "traits" $ do
  el "h1" $ text "Traits"
  el "ul" . trait traitsDyn "Deftness" traitsDeftness $ \tDyn tDsDyn ->
    concentration "Shootin" $ do
      let sDyn = (view (traitAptitudes.deftnessShootin) <$> tDyn)
      let cDyn = concentrationExtract tDsDyn sDyn shootinShotgun
      void . dyn $ (traverse_ (aptitude "Shotgun" . constDyn) <$> cDyn)
  el "ul" . trait traitsDyn "Nimbleness" traitsNimbleness $ \tDyn tDsDyn ->
    concentration "Fightin" $ do
      let fDyn = (view (traitAptitudes.nimblenessFightin) <$> tDyn)
      let cDyn = concentrationExtract tDsDyn fDyn fightinBrawling
      void . dyn $ (traverse_ (aptitude "Brawlin" . constDyn) <$> cDyn)
    --aptitude "Climbin" ((view (traitAptitudes.nimblenessFightin) <$> tDyn)


info :: (PostBuild t m, DomBuilder t m) => Dynamic t CharacterBackground -> m ()
info bgDyn = elClass "div" "info" $ do
  el "h2" $dynText (view chrBgName <$> bgDyn)
  elAttr "img" (Map.fromList [("src",static @"St_Teresa.jpg"), ("width","125")]) blank
  el "dl" $ do
    el "dt" $ text "Occupation"
    el "dd" $ dynText (view chrBgOccupation <$> bgDyn)
    el "dt" $ text "Hometown"
    el "dd" $ dynText (view chrBgHomeTown <$> bgDyn)
    el "dt" $ text "Age"
    el "dd" $ dynText (view (chrBgAge . to show . packed) <$> bgDyn)

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
