{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Frontend.Wounds where

import           Control.Lens

import           Control.Monad.Fix       (MonadFix)
import           Data.Bool               (bool)
import           Data.Functor            (void)
import qualified Data.Map                as Map
import           Data.Maybe              (fromMaybe, maybe)
import           Data.Monoid.Endo        (Endo (Endo))
import           Data.Number.Nat         (Nat, fromNat, toNat)
import           Data.Number.Nat1        (Nat1, fromNat1)
import qualified Data.Text               as T
import           Reflex.Dom

--import           Frontend.Internal          (fget)
--import           Obelisk.Generated.Static

import           Common.CharacterSheet
import           Frontend.Internal       (fget)

data LimbTarget   = LeftLeg | RightLeg | Torso | LeftArm | RightArm | Head deriving (Ord, Eq)
data DamageTarget = Wind | Wounds | Damage deriving (Eq, Ord)
data Damage = WindDamage Integer | WoundDamage LimbTarget Integer | RawDamage LimbTarget Nat

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


makeDamage :: DamageTarget -> Maybe LimbTarget -> Integer -> Maybe Damage
makeDamage dt ltMay l = case dt of
  -- TODO: When you take -maxWounds wind damage, you take a guts wound.
  -- TODO: At the end of every round when you have wound mod > 2, take 1 wind dmg each round
  Wounds -> WoundDamage <$> ltMay <*> pure l
  Damage -> RawDamage <$> ltMay <*> (if l < 0 then Nothing else (Just $ toNat l))
  Wind   -> Just $ WindDamage l

applyDamage :: CharSize -> LightArmor -> Damage -> Endo CharacterHealth
applyDamage cs la = \case
  (WindDamage d)     -> Endo $ chrHealthWindDamage %~ clampModNat (+ d)
  (WoundDamage lt d) -> Endo $
    chrHealthLimbDamage.(limbTargetSetter lt) %~ clampModNat (+d)
  (RawDamage lt d)   -> Endo $
    chrHealthLimbDamage.(limbTargetSetter lt) %~ clampModNat (+ (fromNat (damageToWounds cs la d)))
  where
    limbTargetSetter :: LimbTarget -> ASetter' LimbDamage Nat
    limbTargetSetter = \case
      Head     -> limbDamageHead
      Torso    -> limbDamageTorso
      LeftArm  -> limbDamageLeftArm._Just
      RightArm -> limbDamageRightArm._Just
      LeftLeg  -> limbDamageLeftLeg._Just
      RightLeg -> limbDamageRightLeg._Just

wounds
  :: (MonadHold t m, MonadFix m, PostBuild t m, DomBuilder t m, DomBuilderSpace m ~ GhcjsDomSpace)
  => Dynamic t CharSize
  -> Dynamic t Nat1
  -> Dynamic t LightArmor
  -> Dynamic t CharacterHealth
  -> m (Event t (Endo CharacterHealth))
wounds sizeDyn maxWindDyn lightArmorDyn healthDyn = elClass "div" "wounds-tracker" $ mdo
  el "h2" $ text "Wounds"
  let
    woundsHeadDyn     = fget (chrHealthLimbDamage.limbDamageHead) healthDyn
    woundsTorsoDyn    = fget (chrHealthLimbDamage.limbDamageTorso) healthDyn
    woundsLeftArmDyn  = fget (chrHealthLimbDamage.limbDamageLeftArm) healthDyn
    woundsRightArmDyn = fget (chrHealthLimbDamage.limbDamageRightArm) healthDyn
    woundsLeftLegDyn  = fget (chrHealthLimbDamage.limbDamageLeftLeg) healthDyn
    woundsRightLegDyn = fget (chrHealthLimbDamage.limbDamageRightLeg) healthDyn
    deadDyn           = fget (chrHealthLimbDamage.to isDead) healthDyn
    maxWoundsDyn      = fget (chrHealthLimbDamage.to maxWounds) healthDyn
    windDyn           = (\m d -> (fromNat1 m - fromNat d)::Integer)
                          <$> maxWindDyn
                          <*> fget chrHealthWindDamage healthDyn
  elClass "div" "wounds-container" $ do
    let wounds' = elClass "div" "wounds" $ do
          elDynAttr "div" (("class" =:) . woundCssClass Head <$> woundsHeadDyn)     blank
          elDynAttr "div" (("class" =:) . woundCssClass Torso <$> woundsTorsoDyn)    blank
          elDynAttr "div" (("class" =:) . woundCssClass LeftArm . fromMaybe 5 <$> woundsLeftArmDyn)  blank
          elDynAttr "div" (("class" =:) . woundCssClass RightArm . fromMaybe 5 <$> woundsRightArmDyn) blank
          elDynAttr "div" (("class" =:) . woundCssClass LeftLeg . fromMaybe 5 <$> woundsLeftLegDyn)  blank
          elDynAttr "div" (("class" =:) . woundCssClass RightLeg . fromMaybe 5 <$> woundsRightLegDyn) blank
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
      el "span" $ display (unLightArmor <$> lightArmorDyn)
    el "div" $ do
      el "span" $ text "Size: "
      el "span" $ display (unCharSize <$> sizeDyn)
  dmgE <- elClass "div" "damage-form" $ do
    let damageOptsDyn = constDyn (Map.fromList [(Damage,"Damage"),(Wind,"Wind"),(Wounds,"Wounds")])
    damageDd <- dropdown Damage damageOptsDyn $ def
    let
      calcTargets dt = Map.fromList $ case dt of
        Wind -> [(Nothing, "None")]
        _    -> [(Nothing, "None"),(Just Head,"Head"),(Just Torso,"Torso")
                ,(Just LeftArm, "Left Arm"),(Just RightArm, "Right Arm")
                ,(Just LeftLeg, "Left Leg"),(Just RightLeg,"Right Leg")
                ]
      targetOptsDyn = calcTargets <$> damageDd^.dropdown_value
    targetDd <- dropdown Nothing targetOptsDyn $ def
    ti <- textInput $ def
      & textInputConfig_initialValue .~ "0"
      & textInputConfig_inputType    .~ "number"
    let dmgLevelDyn = read . T.unpack <$> (ti^.textInput_value)
    let damageDyn = makeDamage <$> damageDd^.dropdown_value <*> targetDd^.dropdown_value <*> dmgLevelDyn
    applyE <- do
      (bElt,_) <- elDynAttr' "button"
        (maybe ("disabled" =: "") (const Map.empty) <$> damageDyn )
        (text "Apply")
      pure $ domEvent Click bElt
    pure . fmapMaybe id $ (current damageDyn) <@ applyE
  pure $ applyDamage <$> current sizeDyn <*> current lightArmorDyn <@> dmgE
