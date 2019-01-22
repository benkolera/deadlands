{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Frontend where

import           Control.Lens

import           Clay                       (render)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Fix          (MonadFix)
import           Data.Bool                  (bool)
import           Data.Foldable              (traverse_)
import qualified Data.Map                   as Map
import           Data.Number.Nat            (Nat)
import           Data.Number.Nat1           (toNat1)
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as TL
import           Data.Text.Lens             (packed)
import qualified GHCJS.DOM.HTMLInputElement as JsInput
import           GHCJS.DOM.Types            (MonadJSM, liftJSM)
import           Obelisk.Frontend
import           Obelisk.Route
import           Reflex.Dom
import           Reflex.Dom.Core

import           Common.CharacterSheet
import           Common.DiceSet
import           Common.Route
import           Frontend.Style
import           Obelisk.Generated.Static

fget :: Functor f => Getter a b -> f a -> f b
fget g = fmap (view g)

traitName
  :: (MonadHold t m, MonadFix m, PostBuild t m, DomBuilder t m
     , DomBuilderSpace m ~ GhcjsDomSpace, PerformEvent t m, MonadJSM (Performable m))
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
     , DomBuilderSpace m ~ GhcjsDomSpace, PerformEvent t m)
  => Dynamic t DiceSet
  -> Bool
  -> m ()
diceCodeRoller dsDyn forTrait = do
  elClass "div" "dicecode-container" $ mdo
    (imgElt,_) <- elAttr' "img" (Map.fromList [("src",static @"dice.svg"),("class","dice-icon")]) blank
    let openE = True <$ domEvent Click imgElt
    modalOpenedDyn <- foldDyn const False (leftmost [openE, (False <$ closeE)])
    closeE <- elDynClass "div" (bool "modal" "modal opened" <$> modalOpenedDyn) $ do
      elClass "div" "modal-content" $ do
        dsInit <- sample . current $ dsDyn
        tiElt <- textInput $ def
          & textInputConfig_initialValue .~ (T.pack . show $ dsInit)
          & textInputConfig_setValue     .~ ((T.pack . show) <$> updated dsDyn)

        copyE <- button "copy"
        let rawElt = tiElt ^. textInput_builderElement.to _inputElement_raw
        performEvent_ $ (JsInput.select rawElt) <$ copyE
        button "close"
    pure ()

aptitude
  :: (MonadFix m, MonadHold t m, PostBuild t m, DomBuilder t m
     , DomBuilderSpace m ~ GhcjsDomSpace, PerformEvent t m
     , MonadJSM (Performable m))
  => T.Text
  -> Dynamic t Nat
  -> Dynamic t (Trait z)
  -> m ()
aptitude aName levelDyn tDyn = elClass "li" "aptitude" $ do
  let aDsDyn = aptitudeDice <$> (fget traitDiceSet tDyn) <*> levelDyn
  elClass "span" "name" $ text aName
  elClass "span" "value" . display $ aDsDyn
  diceCodeRoller aDsDyn False

aptitudeDice :: DiceSet -> Nat -> DiceSet
aptitudeDice ds 0 = ds & diceSetNum .~ 1 & diceSetBonus %~ (\x -> x - 4)
aptitudeDice ds n = ds & diceSetNum .~ (toNat1 n)

concentrationAptitude
  :: (MonadHold t m, MonadFix m, PostBuild t m, DomBuilder t m
     , DomBuilderSpace m ~ GhcjsDomSpace, MonadJSM (Performable m)
     , PerformEvent t m)
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
     , PerformEvent t m)
  => T.Text
  -> Getter a Nat
  -> Dynamic t (Trait a)
  -> m ()
pureAptitude name getter tDyn =
  aptitude name (fget (traitAptitudes.getter) tDyn) tDyn

trait
  :: (MonadHold t m, MonadFix m, PostBuild t m, DomBuilder t m
     , DomBuilderSpace m ~ GhcjsDomSpace, PerformEvent t m
     , MonadJSM (Performable m))
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
    , MonadJSM (Performable m))
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
      , pureAptitude "Guts" spiritFaith
      ]

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
