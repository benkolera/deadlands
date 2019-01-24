{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Frontend.Traits where

import           Control.Lens

import           Control.Monad              (when)
import           Control.Monad.Fix          (MonadFix)
import           Data.Bool                  (bool)
import           Data.Foldable              (traverse_)
import qualified Data.Map                   as Map
import           Data.Number.Nat            (Nat)
import           Data.Number.Nat1           (toNat1)
import qualified Data.Text                  as T
import qualified GHCJS.DOM.Document         as JsDocument
import qualified GHCJS.DOM.HTMLInputElement as JsInput
import           GHCJS.DOM.Types            (MonadJSM)
import           Reflex.Dom

import           Common.CharacterSheet
import           Common.DiceSet
import           Frontend.Internal          (fget)
import           Obelisk.Generated.Static

traitName
  :: (MonadHold t m, MonadFix m, PostBuild t m, DomBuilder t m
     , DomBuilderSpace m ~ GhcjsDomSpace, PerformEvent t m, MonadJSM (Performable m), HasDocument m)
  => T.Text
  -> Dynamic t DiceSet
  -> m ()
traitName n dsDyn = do
  elClass "span" "trait-name" $ do
    text n
  elClass "span" "trait-dice" $ do
    display dsDyn
    diceCodeRoller dsDyn True

concentration
  :: (PostBuild t m, DomBuilder t m)
  => T.Text
  -> Getter a (Concentration b)
  -> [Dynamic t (Concentration b) -> Dynamic t (Trait a) -> m ()]
  -> Dynamic t (Trait a)
  -> m ()
concentration cn getter childLis tDyn = do
  el "li" $ do
    text cn
    elClass "ul" "concentration" $ do
      let cDyn = fget (traitAptitudes.getter) tDyn
      traverse_ (\f -> f cDyn tDyn) childLis

diceCodeRoller
  :: (MonadFix m, MonadHold t m, DomBuilder t m, PostBuild t m, MonadJSM (Performable m)
     , DomBuilderSpace m ~ GhcjsDomSpace, PerformEvent t m, HasDocument m)
  => Dynamic t DiceSet
  -> Bool
  -> m ()
diceCodeRoller tdsDyn forTrait = do
  elClass "div" "dicecode-container" $ mdo
    (imgElt,_) <- elAttr' "img" (Map.fromList [("src",static @"dice.svg"),("class","dice-icon")]) blank
    let openE = True <$ domEvent Click imgElt
    modalOpenedDyn <- foldDyn const False (leftmost [openE, (False <$ closeE)])
    closeE <- elDynClass "div" (bool "modal" "modal opened" <$> modalOpenedDyn) $ do
      elClass "div" "modal-content" $ do
        tn <- el "label" $ do
          text "TN:"
          textInput $ def
            & textInputConfig_initialValue .~ "5"
            & textInputConfig_inputType    .~ "number"
            & textInputConfig_attributes   .~ (constDyn (Map.fromList [("min","1"),("class","tn-input")]))
        let tnDyn = read . T.unpack <$> (tn^.textInput_value)
        elClass "ul" "dice-codes" $ do
          when forTrait . el "li" $ copyPasta "Untrained" tnDyn (aptitudeDice 0 <$> tdsDyn)
          el "li" $ copyPasta "Normal" tnDyn tdsDyn
        button "close"
    pure ()
  where
    copyPasta label tnDyn dsDyn = elClass "div" "dice-code" $ do
        elClass "div" "dice-code-label" $ text label
        elClass "div" "dice-code-code" $ do
          let dcDyn = (\tn -> toFgCode (SetVsTn (TnCheck tn True))) <$> tnDyn <*> dsDyn
          dcInit <- sample . current $ dcDyn
          ti <- textInput $ def
            & textInputConfig_initialValue .~ dcInit
            & textInputConfig_setValue     .~ updated dcDyn
            & textInputConfig_attributes   .~ (constDyn ("class" =: "copypasta inline"))

          (imgElt,_) <- elAttr' "img" (Map.fromList [("src",static @"copy-content.svg"),("class","copy-icon")]) blank
          let copyE = True <$ domEvent Click imgElt
          let rawElt = ti ^. textInput_builderElement.to _inputElement_raw
          doc <- askDocument
          performEvent_ $ copyToClipboard rawElt doc <$ copyE
    copyToClipboard elt doc = do
      JsInput.select elt
      -- Watch out, this can only be executed in a user event so it wont work
      -- in ob run. Sadness.
      JsDocument.execCommand_ doc ("copy"::String) False (Nothing::Maybe String)
      pure ()

aptitude
  :: (MonadFix m, MonadHold t m, PostBuild t m, DomBuilder t m
     , DomBuilderSpace m ~ GhcjsDomSpace, PerformEvent t m
     , MonadJSM (Performable m), HasDocument m)
  => T.Text
  -> Dynamic t Nat
  -> Dynamic t (Trait z)
  -> m ()
aptitude aName levelDyn tDyn = elClass "li" "aptitude" $ do
  let aDsDyn = aptitudeDice <$> levelDyn <*> (fget traitDiceSet tDyn)
  elClass "span" "name" $ text aName
  elClass "span" "value" . display $ aDsDyn
  diceCodeRoller aDsDyn False

aptitudeDice :: Nat -> DiceSet -> DiceSet
aptitudeDice 0 ds = ds & diceSetNum .~ 1 & diceSetBonus %~ (\x -> x - 4)
aptitudeDice n ds = ds & diceSetNum .~ (toNat1 n)

concentrationAptitude
  :: (MonadHold t m, MonadFix m, PostBuild t m, DomBuilder t m
     , DomBuilderSpace m ~ GhcjsDomSpace, MonadJSM (Performable m)
     , PerformEvent t m, HasDocument m)
  => T.Text
  -> Getter a Bool
  -> Dynamic t (Concentration a)
  -> Dynamic t (Trait z)
  -> m ()
concentrationAptitude label g cDyn tDyn = do
  let alDyn = (\c ->
                bool
                0
                (c^.concentrationLevel)
                (c^.concentrationAptitudes.g)
             )
        <$> cDyn
  aptitude label alDyn tDyn

pureAptitude
  :: (MonadHold t m, MonadFix m, PostBuild t m, DomBuilder t m
     , DomBuilderSpace m ~ GhcjsDomSpace, MonadJSM (Performable m)
     , PerformEvent t m, HasDocument m)
  => T.Text
  -> Getter a Nat
  -> Dynamic t (Trait a)
  -> m ()
pureAptitude name getter tDyn =
  aptitude name (fget (traitAptitudes.getter) tDyn) tDyn

trait
  :: (MonadHold t m, MonadFix m, PostBuild t m, DomBuilder t m
     , DomBuilderSpace m ~ GhcjsDomSpace, PerformEvent t m
     , MonadJSM (Performable m), HasDocument m)
  => Dynamic t s
  -> T.Text
  -> Getting (Trait a) s (Trait a)
  -> [Dynamic t (Trait a) -> m ()]
  -> m ()
trait traitsDyn tLabel tGetter aptitudes =
  elClass "li" "trait" $ do
    let tDyn = view tGetter <$> traitsDyn
    traitName tLabel (fget traitDiceSet tDyn)
    elClass "ul" "aptitudes" $ traverse_ ($ tDyn) aptitudes

traits
  ::(MonadHold t m, MonadFix m, PostBuild t m, DomBuilder t m
    , DomBuilderSpace m ~ GhcjsDomSpace, PerformEvent t m
    , MonadJSM (Performable m), HasDocument m)
  => Dynamic t Traits
  -> m ()
traits traitsDyn = elClass "div" "traits" $ do
  el "h1" $ text "Traits"
  el "ul" $ do
    trait traitsDyn "Deftness" traitsDeftness $
      [ concentration "Shootin" deftnessShootin
        [ concentrationAptitude "Shotgun" shootinShotgun
        ]
      ]
    trait traitsDyn "Nimbleness" traitsNimbleness $
      [ concentration "Fightin" nimblenessFightin
        [ concentrationAptitude "Brawlin" fightinBrawlin
        ]
      , pureAptitude "Climbin" nimblenessClimbin
      , pureAptitude "Dodge" nimblenessDodge
      ]
    trait traitsDyn "Quickness" traitsQuickness $
      [ concentration "Quick Load" quicknessQuickLoad
        [ concentrationAptitude "Shotgun" quickLoadShotgun
        ]
      ]
    trait traitsDyn "Strength" traitsStrength $
      []
    trait traitsDyn "Vigor" traitsVigor $
      []
    trait traitsDyn "Cognition" traitsCognition $
      [ pureAptitude "Search" cognitionSearch
      , pureAptitude "Scruitinize" cognitionScruitinize
      ]
    trait traitsDyn "Knowledge" traitsKnowledge $
      [ pureAptitude "Occult" knowledgeOccult
      , pureAptitude "Theology" knowledgeTheology
      , pureAptitude "Latin" knowledgeLatin
      , pureAptitude "English" knowledgeEnglish
      , pureAptitude "Spanish" knowledgeSpanish
      , pureAptitude "Area: Chihuahua" knowledgeChihuahua
      ]
    trait traitsDyn "Mien" traitsMien $
      [ pureAptitude "Overawe" mienOverawe
      ]
    trait traitsDyn "Smarts" traitsSmarts $
      [ pureAptitude "Streetwise" smartsStreetwise
      ]
    trait traitsDyn "Spirit" traitsSpirit $
      [ pureAptitude "Faith" spiritFaith
      , pureAptitude "Guts" spiritGuts
      ]
