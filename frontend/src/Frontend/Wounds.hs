{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Frontend.Wounds where

--import           Control.Lens

import           Data.Bool               (bool)
import           Data.Functor            (void)
import           Data.List.NonEmpty      (NonEmpty ((:|)))
--import qualified Data.Map                   as Map
import           Data.Maybe              (fromMaybe, maybe)
import           Data.Number.Nat         (Nat)
import           Data.Number.Nat1        (Nat1)
import           Data.Semigroup          (Max (Max, getMax))
import           Data.Semigroup.Foldable (foldMap1)
import qualified Data.Text               as T
import           Reflex.Dom

--import           Frontend.Internal          (fget)
--import           Obelisk.Generated.Static

data LimbTarget   = LeftLeg | RightLeg | Torso | LeftArm | RightArm | Head
data DamageTarget = Wind | Wounds LimbTarget

woundDesc :: Nat -> Maybe T.Text
woundDesc n = case n of
  0 -> Nothing
  1 -> Just "Light"
  2 -> Just "Heavy"
  3 -> Just "Serious"
  4 -> Just "Critical"
  _ -> Just "Maimed"

woundCssClass :: LimbTarget -> Nat -> T.Text
woundCssClass lt level = limbCssClass <> maybe "" ((" " <>) . T.toLower) (woundDesc level)
  where
    limbCssClass = case lt of
      Head     -> "head"
      Torso    -> "torso"
      LeftArm  -> "larm"
      RightArm -> "rarm"
      LeftLeg  -> "lleg"
      RightLeg -> "rleg"

wounds
  :: (MonadHold t m, PostBuild t m, DomBuilder t m)
  => Dynamic t Nat1
  -> Dynamic t Nat1
  -> Dynamic t Nat
  -> m () -- TODO : Output dynamic of effects
wounds sizeDyn maxWindDyn lightArmorDyn = elClass "div" "wounds-tracker" $ do
  el "h2" $ text "Wounds"
  let deadDyn           = constDyn False
  let headWoundsDyn     = constDyn (1::Nat)
  let torsoWoundsDyn    = constDyn (2::Nat)
  let leftArmWoundsDyn  = constDyn (Just (3::Nat))
  let rightArmWoundsDyn = constDyn (Just (4::Nat))
  let leftLegWoundsDyn  = constDyn (Just (4::Nat))
  let rightLegWoundsDyn = constDyn Nothing
  let maxWoundsDyn      = fmap (getMax . foldMap1 Max) . sequence $ headWoundsDyn :|
        [ torsoWoundsDyn
        , fromMaybe 0 <$> leftArmWoundsDyn
        , fromMaybe 0 <$> rightArmWoundsDyn
        , fromMaybe 0 <$> leftLegWoundsDyn
        , fromMaybe 0 <$> rightLegWoundsDyn
        ]
  let windDyn        = maxWindDyn
  -- let woundE       = never

  elClass "div" "wounds-container" $ do
    let wounds' = elClass "div" "wounds" $ do
          elDynAttr "div" (("class" =:) . woundCssClass Head     <$> headWoundsDyn)     blank
          elDynAttr "div" (("class" =:) . woundCssClass Torso    <$> torsoWoundsDyn)    blank
          elDynAttr "div" (("class" =:) . woundCssClass LeftArm . fromMaybe 5 <$> leftArmWoundsDyn)  blank
          elDynAttr "div" (("class" =:) . woundCssClass RightArm . fromMaybe 5 <$> rightArmWoundsDyn) blank
          elDynAttr "div" (("class" =:) . woundCssClass LeftLeg . fromMaybe 5 <$> leftLegWoundsDyn)  blank
          elDynAttr "div" (("class" =:) . woundCssClass RightLeg . fromMaybe 5 <$> rightLegWoundsDyn) blank
    let dead = elClass "div" "dead" $ blank
    void $ dyn (bool wounds' dead <$> deadDyn)
  elClass "div" "physical-deets" $ do
    el "div" $ do
      el "span" $ text "Wind: "
      el "span" $ do
        display windDyn
        text "/"
        display maxWindDyn
    el "div" $ do
      el "span" $ text "Wound Modifier: "
      el "span" $ do
        display $ ((0::Integer)-) . fromIntegral <$> maxWoundsDyn
        dynText $ maybe "" (\d -> "(" <> d <> ")") . woundDesc <$> maxWoundsDyn
    el "div" $ do
      el "span" $ text "Light Armor: "
      el "span" $ display lightArmorDyn
    el "div" $ do
      el "span" $ text "Size: "
      el "span" $ display sizeDyn
  elClass "div" "damage-form" $ do
    blank
