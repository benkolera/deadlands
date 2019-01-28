{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Frontend.Wounds where

import           Control.Lens

import           Control.Lens.TH         (makeLenses)
import           Control.Monad.Fix       (MonadFix)
import           Data.Bool               (bool)
import           Data.Functor            (void)
import           Data.List.NonEmpty      (NonEmpty ((:|)))
import qualified Data.Map                   as Map
import           Data.Maybe              (fromMaybe, maybe)
import           Data.Number.Nat         (Nat, toNat)
import           Data.Number.Nat1        (Nat1, fromNat1)
import           Data.Semigroup          (Max (Max, getMax))
import           Data.Semigroup.Foldable (foldMap1)
import qualified Data.Text               as T
import           Reflex.Dom

--import           Frontend.Internal          (fget)
--import           Obelisk.Generated.Static

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

data Limbs = Limbs
  { _limbsHead     :: Nat
  , _limbsTorso    :: Nat
  , _limbsLeftArm  :: (Maybe Nat)
  , _limbsRightArm :: (Maybe Nat)
  , _limbsLeftLeg  :: (Maybe Nat)
  , _limbsRightLeg :: (Maybe Nat)
  }
makeLenses ''Limbs

changeWounds :: LimbTarget -> (Nat1, Damage) -> Nat -> Nat
changeWounds et (s,dd) = case dd of
  (WindDamage _)      -> id
  (WoundDamage lt w) -> if (lt == et) then (clampMod (+ w)) else id
  (RawDamage lt d)    -> if (lt == et) then (clampMod (+ (fromIntegral $ damageToWounds d))) else id
  where
    clampMod :: (Integer -> Integer) -> Nat -> Nat
    clampMod f n = if r < 0 then 0 else if r > 5 then 5 else toNat r
      where r = f . fromIntegral $ n
    damageToWounds :: Nat -> Nat
    damageToWounds d  = toNat @Integer $ (fromIntegral d) `div` (fromNat1 s)

changeWoundsLimb :: LimbTarget -> (Nat1, Damage) -> Maybe Nat -> Maybe Nat
changeWoundsLimb lt t = fmap (changeWounds lt t)

changeWind :: (Nat1, Damage) -> Integer -> Integer
changeWind (maxWind, (WindDamage l)) = (\x -> min (fromNat1 maxWind) (x - l))
changeWind _                         = id

makeDamage :: DamageTarget -> Maybe LimbTarget -> Integer -> Maybe Damage
makeDamage dt ltMay l = case dt of
  Wounds -> WoundDamage <$> ltMay <*> pure l
  Damage -> RawDamage <$> ltMay <*> (if l < 0 then Nothing else (Just $ toNat l))
  Wind   -> Just $ WindDamage l

wounds
  :: (MonadHold t m, MonadFix m, PostBuild t m, DomBuilder t m, DomBuilderSpace m ~ GhcjsDomSpace)
  => Dynamic t Nat1
  -> Dynamic t Nat1
  -> Dynamic t Nat
  -> Limbs
  -> m (Dynamic t Nat)
wounds sizeDyn maxWindDyn lightArmorDyn limbs = elClass "div" "wounds-tracker" $ mdo
  el "h2" $ text "Wounds"
  let dmgSizeE = attach (current sizeDyn) dmgE
  woundsHeadDyn     <- foldDyn (changeWounds Head) (limbs^.limbsHead) dmgSizeE
  woundsTorsoDyn    <- foldDyn (changeWounds Torso) (limbs^.limbsTorso) $ dmgSizeE
  woundsLeftArmDyn  <- foldDyn (changeWoundsLimb LeftArm) (limbs^.limbsLeftArm) $ dmgSizeE
  woundsRightArmDyn <- foldDyn (changeWoundsLimb RightArm) (limbs^.limbsRightArm) $ dmgSizeE
  woundsLeftLegDyn  <- foldDyn (changeWoundsLimb LeftLeg) (limbs^.limbsLeftLeg) dmgSizeE
  woundsRightLegDyn <- foldDyn (changeWoundsLimb RightLeg) (limbs^.limbsRightLeg) dmgSizeE
  windMaxInit       <- sample . current $ maxWindDyn
  windDyn           <- foldDyn changeWind (fromNat1 windMaxInit) $ attach (current maxWindDyn) dmgE
  let
    maxWounds = fmap (getMax . foldMap1 Max) . sequence
    maxWoundsDyn = maxWounds $ woundsHeadDyn :|
        [ woundsTorsoDyn
        , fromMaybe 0 <$> woundsLeftArmDyn
        , fromMaybe 0 <$> woundsRightArmDyn
        , fromMaybe 0 <$> woundsLeftLegDyn
        , fromMaybe 0 <$> woundsRightLegDyn
        ]
    deadDyn = (> 4) <$> maxWounds (woundsHeadDyn :| [woundsTorsoDyn])

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
      el "span" $ display lightArmorDyn
    el "div" $ do
      el "span" $ text "Size: "
      el "span" $ display sizeDyn
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
  pure $ maxWoundsDyn
