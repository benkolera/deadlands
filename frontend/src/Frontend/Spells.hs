{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Frontend.Spells where

import           Control.Lens

import           Control.Applicative   (liftA2)
import           Control.Monad.Fix     (MonadFix)
import           Data.Bool             (bool)
import           Data.Dependent.Map    (DMap, DSum ((:=>)))
import qualified Data.Dependent.Map    as DMap
import           Data.Foldable         (traverse_, toList)
import           Data.Functor          (void)
import           Data.List.NonEmpty    (nonEmpty)
import           Data.Map              (Map)
import qualified Data.Map              as Map
import           Data.Monoid.Endo      (Endo(Endo))
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Reflex.Dom

import           Common.CharacterSheet
import           Common.DiceSet
import           Frontend.Internal     (fget)

blessingsMap :: Traits DiceSet -> BlessingsMap -> Map EffectName (EffectMeta Blessings)
blessingsMap ts = Map.fromList . fmap blessingMeta . DMap.toList
  where
    blessingMeta :: DSum Blessings Identity -> (EffectName, EffectMeta Blessings)
    blessingMeta e = case e of
      (ArmorOfRighteousness :=> Identity mab) -> toEffectTuple "Armor o' Righteousness"
        (AptitudeCheckEffect 5 1
         (ts^.traitsSpirit.traitAptitudes.spiritFaith)
         mab
         (Endo . DMap.insert ArmorOfRighteousness . Identity)
        )
        "Light armor bonus for a round"
        [ "Lately, good folks have been in short supply. Part of the problem is the good are always getting picked on by the bad—and the ugly as well. Recognizing it’s hard to fight the good fight from six feet under, the divine patrons of blessed heroes have given them a bit of holy reinforcement."
        , "This miracle provides the hero with protection from wounds. The result of his roll to invoke armor o’ righteousness is subtracted from any damage done to him during the round. Then, wound levels are calculated normally from the remaining damage."
        , "Armor o’ righteousness provides this protection against all damage suffered during the round. Additional effects from the attack that rely on damage being dealt (such as a rattlesnake’s poison) are lost if armor negates enough damage to prevent all wound levels."
        , "Once the damage has been reduced by the armor, any wound levels caused by the remaining damage incur Wind loss as normal. However, against brawling attacks, which normally cause only Wind loss, the armor does reduce the amount of Wind lost."
        ]
      (Smite :=> Identity mab) -> toEffectTuple "Smite"
        (AptitudeCheckEffect 5 20
         (ts^.traitsSpirit.traitAptitudes.spiritFaith)
         mab
         (Endo . DMap.insert Smite . Identity)
        )
        "Improve strength dice face for 1 minute."
        [ "With this miracle, the blessed heroes of the Weird West can smite the evils of the Reckoning back into the last century."
        , "When invoked, the invoker’s Strength die type is raised +1 step for every success."
        ]
      (Chastise :=> _) -> toEffectTuple "Chastise" PassiveEffect
        "Use Faith roll for Overawe."
        [ "Hellfire and brimstone preacher when he’s berating a sinner. Even the toughest gunslingers are likely to back down under such abuse. And, just to be sure, chastise gives the blessed an extra boost in a shouting match."
        , "Chastise allows the blessed to use her faith Aptitude in place of the overawe Aptitude in a test of wills as described in the Deadlands rulebook."
        , "When this miracle is invoked, the blessed makes an opposed roll of her faith Aptitude versus her opponent’s guts. Any modifiers she would normally receive to overawe apply to this faith roll as well. If the blessed wins the roll, the subject suffers the effects of the test of wills, becoming unnerved, distracted, or even broken, depending on the level of the blessed’s success."
        ]
      (RefugeOFaith :=> _) -> toEffectTuple "Refuge o' Faith" PassiveEffect
        "Use faith roll for Dodge"
        [ "All blessed heroes may not have a guardian angel looking after them, but they all have a whole lot of faith their deity will protect them from harm. This miracle goes a long way toward proving that belief well-founded."
        , "This gift lets the character use his faith Aptitude as an active defense in place of dodge or fightin’. Whenever he wants to try to avoid an attack, he follows the normal procedures for active defenses, except he rolls his faith instead of his dodge or fightin’ Aptitude. His attacker’s TN to hit him is now the greater of either his normal TN or the blessed’s faith roll."
        , "Of course, just like with any active defense, the character must spend his highest card to use this miracle. As always, any card up the hero’s sleeve is considered his highest."
        ]
      (LayOnHands :=> _) -> toEffectTuple "Lay on Hands" PassiveEffect
        "Slow faith based healing"
        [ "Holy healers have been around since ancient times. They’ve just never been in such demand."
        , "The blessed use this miracle to heal the wounds and afflictions of others (not themselves). The problem is that if the healer is not truly faithful, he takes on the subject’s malady as well."
        , "The base TN for healing a subject’s wounds is shown on the chart below. Use the TN for the highest wound level the victim has sustained. The entry for “maimed” applies to severed limbs, diseases, blindness, and other extremely serious maladies. Maimed gizzards and noggins cannot be healed with this miracle. Working with dead folks takes a bit more doing."
        , "The blessed cannot bring back the truly dead with this miracle. That takes quite a bit more effort. Nor can the miracle heal the undead, like the Harrowed. Once a person’s breathed his last, there’s little that can be done for him."
        , "The blessed actually feels the victim’s pain, so she must subtract the patient’s total wound modifier from her roll. As usual, only the highest wound modifier applies, so if the blessed is already suffering a larger wound modifier of her own, she doesn’t suffer any additional effects."
        , "If the healer is successful, the victim is completely healed in all areas of his body. The patient maintains his wound modifiers for the next hour (due to stiffness in the healed areas), but he is not otherwise considered wounded."
        , "The bad news is that if the healer fails the roll, the patient isn’t cured, and the healer takes on the same maladies or wounds. If these are wounds, the blessed takes the victim’s highest wound level to the corresponding hit location on her own body. For example, if the victim’s highest wound level was a serious wound on his left arm, the blessed would take a serious wound to her own left arm, but she wouldn’t be affected by any of the patient’s other injuries."
        ]
      (HolyRoller :=> _) -> toEffectTuple "Holy Roller" PassiveEffect
        "Get a chip that must be used next action"
        [ "The blessed know asking for holy power is normally off-limits. But sometimes the horrors of the Weird West call for desperate measures. Right then, a little prayer for help can make all the difference."
        , "A blessed character can use this miracle to gain a chip from the Fate Pot. The chip must be used on her next action. If the character meets the difficulty, she gains a white chip. A raise nets her a red chip, and 2 raises gets her a blue chip."
        , "The chip can only be used for Trait or Aptitude checks or to avoid damage. For example, while it can’t be spent for Bounty Points, used as a sacrifice for consecrate weapon, or to activate a knack, but it could be used to help the blessed make her lay on hands roll."
        , "A chip gained in this manner can be given to another player with the sacrifice miracle, however, the receiving character must then use the chip on his next action. Likewise, the chip can only be spent to avoid damage or assist a Trait or Aptitude check."
        , "The downside is that if the blessed miracle worker fails the roll, her patron takes her highest chip (put it back in the pot) as penance. It’s a gamble—that’s why the blessed call this miracle “holy roller.” Of course, if she doesn’t have a chip, she can ignore this effect."
        ]
      (Protection :=> _) -> toEffectTuple "Protection" PassiveEffect
        "Opposed spirit check that prevents harm from supernatural evil."
        [ "One miracle used by all western religions is protection. This is simply reliance on one’s deity or deities to protect the faithful from supernatural evil. Any character with at least one level in the faith Aptitude may attempt this miracle by presenting her holy symbol or otherwise declaring the power of her deity. Like we said before, if your character is a follower of the Indian spirits, you can’t use this miracle, no matter how much faith you’ve got. The spirits do grant favors, just not this particular one."
        , "A supernaturally evil opponent must make a Spirit total versus the hero’s faith. Should it lose, the creature cannot touch the character or otherwise cause her direct harm. It could still push over a bookshelf the blessed happened to be standing under, but it couldn’t fire a weapon, cast a hex, or use its special abilities on her until it wins the spiritual contest."
        , "Of course, this doesn’t do the blessed’s companions a bit of good. They’re still fair game. Truly valiant heroes that have protection often find they can help the rest of their posse by standing directly between the horrific creature and their hapless friends. Be careful, though. This can be a really awkward place to be should the miracle suddenly fail."
        , "Faithful characters shouldn’t rely on this miracle too often, since the winner of the contest between the blessed and the beast is likely to waver back and forth. And any creature affected by protection probably doesn’t need more than one opening to finish the fight. Permanently."
        ]
      (Confession :=> _) -> toEffectTuple "Confession" PassiveEffect
        "It is very difficult to lie to the Nun."
        [ "Lying to a preacher is kind of like lying to your mother. Most folks don’t do it too well, and it usually isn’t a whole lot of fun even when they do. With this miracle, it’s even tougher to pull the wool over a blessed’s eyes."
        , "The blessed chooses a character when she invokes the miracle. If she’s successful, the target finds it incredibly difficult to lie to her."
        , "Any time the target attempts to tell a direct lie to the blessed, he must beat her in an opposed Spirit test against the blessed’s faith ."
        , "The blessed gets a +2 bonus to her faith roll for every raise she gets on her roll to invoke the miracle."
        , "If the target loses, he can’t lie directly. Lies of omission, evasion, or crafty wording are still possible. If he botches his roll, however, he blurts out the truth."
        ]
      (MagicResistant :=> _) -> toEffectTuple "ResistMagic" PassiveEffect
        "Resistant to black magic and hexes."
        [ "One of the most feared weapons of the servants of Darkness is their black magic. Their spells are deadly and easy—a sure recipe for danger! A character with this gift has been granted a special resistance to their accursed magics. Blessed heroes who are magic resistant are born witch hunters."
        , "This gift gives a –4 modifier to the roll to cast any black magic spell cast against the hero. While players really shouldn’t know too much about the abilities of their opponents, let’s just say that’s a fair boost in difficulty!"
        , "Magic resistant also gives some defense against a huckster’s hexes, too. It’s not quite as effective against hexes, but it still saddles the huckster with a –2 to his roll to cast a hex directly at the blessed."
        , "This modifier applies to any hex or black magic spell cast at the hero—even those beneficial to her. The gift can’t be turned off."
        , "It has no effect on miracles or favors, nor does it affect hexes or spells not cast specifically at the character."
        ]

edgesMap :: EdgesMap -> Map EffectName (EffectMeta Edges)
edgesMap = Map.fromList . fmap edgeMeta . DMap.toList
  where
    edgeMeta :: DSum Edges Identity -> (EffectName,EffectMeta Edges)
    edgeMeta e = case e of
      (ArcaneBlessed :=> _) -> toEffectTuple "Arcane (Blessed)" PassiveEffect
        "Gives the ability to cast blessed based spells."
        []
      (Brave :=> _)         -> toEffectTuple "Brave" PassiveEffect
        "+2 to Guts checks"
        [ "Most folks aren’t really brave—they’re just too stupid to know better. Maybe you’re different, but it’s doubtful."
        , "Characters with this Edge add +2 to their guts checks."
        ]
      (LevelHeaded :=> _)  -> toEffectTuple "Level Headed" PassiveEffect
        "When you draw initiative cards, you may discard a card and grab another."
        [ "Veteran gunmen claim speed and skill are vital, but they’re overrated compared to keeping your cool, aiming at your target, and putting it down. A hothead who empties his hogleg too fast soon finds himself taking root in the local bone orchard."
        , "Immediately after drawing Action Cards in combat, a character with this Edge can discard his lowest card and draw another. If the character draws a black Joker on the first draw, he’s out of luck and can’t draw again."
        ]
      (NervesOfSteel :=> _) -> toEffectTuple "Nerves of Steel" PassiveEffect
        "Character may choose not to flee on a failed guts check. Still takes usual penalities."
        [ "Some of the Weird West’s heroes are too darn stubborn to run even when their boots are full of “liquid fear.” Most of their skeletons lie bleaching in the desert, but a few are still fighting the horrors of the High Plains."
        , "Whenever the character fails a guts check and is forced to flee, the character can choose to stand his ground instead. He still suffers any other penalties, however"
        , "A character with nerves o’ steel isn’t necessarily brave. Sometimes he’s just more afraid of being branded a yellowbellied coward than he is of death. Some folks are funny that way."
        ]
      (TheStare :=> _)     -> toEffectTuple "The Stare" PassiveEffect
        "+2 to Overawe as long as the target can see your face (30 feet)"
        [ "There’s something in your stare that makes others nervous. When your eye starts twitching, someone’s about to get carried to Boot Hill. Clint Eastwood has it, and so does your gunslinger."
        , "A character with “the stare” may add +2 to his overawe attacks as long as the intended victim is close enough to look into his steely gaze (usually less than 30 feet)"
        ]

hinderancesMap :: HinderancesMap -> Map EffectName (EffectMeta Hinderances)
hinderancesMap = Map.fromList . fmap hinderanceMeta . DMap.toList
  where
    hinderanceMeta :: DSum Hinderances Identity -> (EffectName, EffectMeta Hinderances)
    hinderanceMeta h = case h of
      (Heroic :=> _) -> toEffectTuple "Heroic" PassiveEffect
        "Cannot refuse a plea for help."
        [ "You’re a sucker for someone in trouble. Ever hear of nice guys finishing last? Heroes who go chasing down wild critters aren’t likely to finish at all. At least they’ll write something nice on your tombstone."
        , "Your character can’t turn down a plea for help. She doesn’t have to be cheery about it, and she doesn’t have to be a “nice” person, but she always helps those in need eventually."
        ]
      (OathChurch :=> _) -> toEffectTuple "Oath (Church)" PassiveEffect
        "Uphold the Catholic Church"
        []
      (Ferner :=> _) -> toEffectTuple "Ferner" PassiveEffect
        "She's not from around here"
        []
      (Poverty :=> _) -> toEffectTuple "Poverty" PassiveEffect
        "Donates money whereever she can. Lives simply."
        []

knacksMap :: KnacksMap -> Map EffectName (EffectMeta Knacks)
knacksMap = Map.fromList . fmap knackMeta . DMap.toList
  where
    knackMeta :: DSum Knacks Identity -> (EffectName, EffectMeta Knacks)
    knackMeta h = case h of
      (BornOnChristmas :=> _) -> toEffectTuple "Born On Christmas" PassiveEffect
        ""
        []

toEffectTuple :: Text -> (EffectMetaMetaValue k) -> Text -> [Text] -> (EffectName, EffectMeta k)
toEffectTuple t emv sd ls = (EffectName t, EffectMeta sd ls emv)

blessings
  :: (MonadHold t m, MonadFix m, PostBuild t m, DomBuilder t m)
  => Dynamic t (Traits DiceSet)
  -> Dynamic t BlessingsMap
  -> m (Event t (Endo BlessingsMap))
blessings tsDyn = effectsSection "Blessings" . liftA2 blessingsMap tsDyn

edges
  :: (MonadHold t m, MonadFix m, PostBuild t m, DomBuilder t m)
  => Dynamic t EdgesMap
  -> m (Event t (Endo EdgesMap))
edges = effectsSection "Edges" . fmap edgesMap

hinderances
  :: (MonadHold t m, MonadFix m, PostBuild t m, DomBuilder t m)
  => Dynamic t HinderancesMap
  -> m (Event t (Endo HinderancesMap))
hinderances = effectsSection "Hinderances" . fmap hinderancesMap

knacks
  :: (MonadHold t m, MonadFix m, PostBuild t m, DomBuilder t m)
  => Dynamic t KnacksMap
  -> m (Event t (Endo KnacksMap))
knacks = effectsSection "Knacks" . fmap knacksMap

effectsSection
  :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m)
  => Text
  -> Dynamic t (Map EffectName (EffectMeta k))
  -> m (Event t (Endo (DMap k Identity)))
effectsSection title eMapDyn = elClass "div" "effects-section" $ do
  el "h2" $ text title
  listEvs <- listWithKey eMapDyn $ \k emDyn -> elClass "div" "effect" $ mdo
    elClass "span" "name" . text . unEffectName $ k
    text ": "
    elClass "span" "short-desc" . dynText . fget effectMetaDesc $ emDyn
    void . dyn $ (fget effectMetaLongDesc emDyn) <&> (traverse_ longDesc . nonEmpty)
    editEv <- dyn $ (fget effectMetaMetaVal emDyn) <&> effectInput
    switchHold never editEv
  pure $ switchDyn (leftmost . toList <$> listEvs)
  where
    effectInput :: (DomBuilder t m) => EffectMetaMetaValue k -> m (Event t (Endo (DMap k Identity)))
    effectInput PassiveEffect = pure never
    effectInput (AptitudeCheckEffect tn rnds ds abMay mkEndo) = do
      text . T.pack . show $ abMay
      buttClick <- button "go"
      pure $ (mkEndo (Just (ActiveBonus 1 3))) <$ buttClick
    longDesc l = mdo
      openDyn <- foldDyn (const not) False toggleE
      (buttEl,_) <- elClass' "button" "toggle-desc" . dynText . fmap (bool "expand" "hide") $ openDyn
      let toggleE = domEvent Click buttEl
      let linePs = elClass "div" "long-desc" $ traverse_ (el "p" . text) l
      -- TODO: Make this an actual form
      let next = bool blank linePs <$> updated openDyn
      void $ widgetHold blank next
      pure ()
