{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Frontend.Traits where

import           Control.Lens
import           Reflex.Dom

import           Control.Monad            (when)
import           Control.Monad.Fix        (MonadFix)
import           Data.Bool                (bool)
import           Data.Foldable            (traverse_)
import qualified Data.Map                 as Map
import           Data.Maybe               (fromMaybe)
import qualified Data.Text                as T
import           Safe                     (readMay)

import           Common.CharacterSheet
import           Common.DiceSet
import           Frontend.CopyPasta       (copyPasta)
import           Frontend.Internal        (fget)
import           Obelisk.Generated.Static

traitName
  :: (MonadHold t m, MonadFix m, PostBuild t m, DomBuilder t m
     , Prerender js m, PerformEvent t m, HasDocument m
     )
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
  -> Getter (a DiceSet) (Concentration b DiceSet)
  -> [Dynamic t (Concentration b DiceSet) -> m ()]
  -> Dynamic t (Trait a DiceSet)
  -> m ()
concentration cn getter childLis tDyn = do
  el "li" $ do
    text cn
    elClass "ul" "concentration" $ do
      let cDyn = fget (traitAptitudes.getter) tDyn
      traverse_ ($ cDyn) childLis

diceCodeRoller
  :: forall t m js
  .  ( MonadFix m, MonadHold t m, DomBuilder t m, PostBuild t m
     , PerformEvent t m, HasDocument m
     , Prerender js m
     )
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
          inputElement $ (def :: InputElementConfig EventResult t (DomBuilderSpace m))
            & inputElementConfig_initialValue .~ "5"
            & inputElementConfig_elementConfig.elementConfig_initialAttributes .~
              (Map.fromList [("min","1"),("class","tn-input"),("type","number")])
        let tnDyn = fromMaybe 0 . readMay . T.unpack <$> (tn^.to _inputElement_value)
        elClass "ul" "dice-codes" $ do
          when forTrait . el "li" $ copyPastaWithLabel "Untrained" tnDyn tdsDyn
          el "li" $ copyPastaWithLabel "Normal" tnDyn tdsDyn
        button "close"
    pure ()
  where
    copyPastaWithLabel label tnDyn dsDyn = elClass "div" "dice-code" $ do
        elClass "div" "dice-code-label" $ text label
        elClass "div" "dice-code-code" $ copyPasta True tnDyn dsDyn

aptitude
  :: (MonadFix m, MonadHold t m, PostBuild t m, DomBuilder t m
     , PerformEvent t m, Prerender js m
     , HasDocument m)
  => T.Text
  -> Dynamic t DiceSet
  -> m ()
aptitude aName dsDyn = elClass "li" "aptitude" $ do
  elClass "span" "name" $ text aName
  elClass "span" "value" . display $ dsDyn
  diceCodeRoller dsDyn False

concentrationAptitude
  :: (MonadHold t m, MonadFix m, PostBuild t m, DomBuilder t m
     , Prerender js m
     , PerformEvent t m, HasDocument m)
  => T.Text
  -> Getter a Bool
  -> Dynamic t (Concentration a DiceSet)
  -> m ()
concentrationAptitude label g cDyn = do
  let alDyn = (\c ->
                 let ds = c^.concentrationLevel
                 in bool (ds & diceSetNum .~ 1) ds (c^.concentrationAptitudes.g)
              )
        <$> cDyn
  aptitude label alDyn

pureAptitude
  :: (MonadHold t m, MonadFix m, PostBuild t m, DomBuilder t m
     , Prerender js m
     , PerformEvent t m, HasDocument m)
  => T.Text
  -> Getter (a DiceSet) DiceSet
  -> Dynamic t (Trait a DiceSet)
  -> m ()
pureAptitude name getter tDyn =
  aptitude name (fget (traitAptitudes.getter) tDyn)

trait
  :: (MonadHold t m, MonadFix m, PostBuild t m, DomBuilder t m
     , Prerender js m, PerformEvent t m
     , HasDocument m)
  => Dynamic t s
  -> T.Text
  -> Getter s (Trait a DiceSet)
  -> [Dynamic t (Trait a DiceSet) -> m ()]
  -> m ()
trait traitsDyn tLabel tGetter aptitudes =
  elClass "li" "trait" $ do
    let tDyn = view tGetter <$> traitsDyn
    traitName tLabel (fget traitDiceSet tDyn)
    elClass "ul" "aptitudes" $ traverse_ ($ tDyn) aptitudes

traits
  :: ( MonadHold t m, MonadFix m, PostBuild t m, DomBuilder t m
     , Prerender js m, PerformEvent t m
     , HasDocument m
     )
  => Dynamic t (Traits DiceSet)
  -> m ()
traits traitsDyn = do
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
      , pureAptitude "Horse Ridin'" nimblenessHorseRidin
      ]
    trait traitsDyn "Quickness" traitsQuickness $
      [ concentration "Quick Load" quicknessQuickLoad
        [ concentrationAptitude "Shotgun" quickLoadShotgun
        ]
      , concentration "Quick Draw" quicknessQuickDraw
        [ concentrationAptitude "Shotgun" quickDrawShotgun
        ]
      ]
    trait traitsDyn "Strength" traitsStrength $
      []
    trait traitsDyn "Vigor" traitsVigor $
      []
    trait traitsDyn "Cognition" traitsCognition $
      [ pureAptitude "Search" cognitionSearch
      , pureAptitude "Scruitinize" cognitionScruitinize
      , pureAptitude "Trackin" cognitionTrackin
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
