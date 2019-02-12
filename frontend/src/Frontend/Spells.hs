{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Frontend.Spells where

import           Control.Lens

import           Control.Monad.Fix     (MonadFix)
import           Data.Bool             (bool)
import           Data.Foldable         (toList)
import           Data.Foldable         (traverse_)
import           Data.Functor          (void)
import           Data.List.NonEmpty    (nonEmpty)
import           Data.Map              (Map)
import qualified Data.Map              as Map
import           Data.Monoid.Endo      (Endo)
--import           Data.Number.Nat         (Nat, toNat)
--import           Data.Number.Nat1        (Nat1, fromNat1)
import           Data.Set              (Set)
import           Reflex.Dom

import           Frontend.Internal     (fget)
--import           Obelisk.Generated.Static
import           Common.CharacterSheet

edgeMap :: Set Edges -> Map EffectName EffectMeta
edgeMap = Map.fromList . fmap edgeMeta . toList
  where
    edgeMeta e = case e of
      ArcaneBlessed -> toEdgeTuple "Arcane (Blessed)" "Gives the ability to cast blessed based spells." []
      Brave         -> toEdgeTuple
        "Brave" "+2 to Guts checks"
        [ "Most folks aren’t really brave—they’re just too stupid to know better. Maybe you’re different, but it’s doubtful."
        , "Characters with this Edge add +2 to their guts checks."
        ]
      LevelHeaded   -> toEdgeTuple
        "Level Headed" "When you draw initiative cards, you may discard a card and grab another."
        [ "Veteran gunmen claim speed and skill are vital, but they’re overrated compared to keeping your cool, aiming at your target, and putting it down. A hothead who empties his hogleg too fast soon finds himself taking root in the local bone orchard."
        , "Immediately after drawing Action Cards in combat, a character with this Edge can discard his lowest card and draw another. If the character draws a black Joker on the first draw, he’s out of luck and can’t draw again."
        ]
      NervesOfSteel -> toEdgeTuple
        "Nerves of Steel" "Character may choose not to flee on a failed guts check. Still takes usual penalities."
        [ "Some of the Weird West’s heroes are too darn stubborn to run even when their boots are full of “liquid fear.” Most of their skeletons lie bleaching in the desert, but a few are still fighting the horrors of the High Plains."
        , "Whenever the character fails a guts check and is forced to flee, the character can choose to stand his ground instead. He still suffers any other penalties, however"
        , "A character with nerves o’ steel isn’t necessarily brave. Sometimes he’s just more afraid of being branded a yellowbellied coward than he is of death. Some folks are funny that way."
        ]
      TheStare      -> toEdgeTuple
        "The Stare" "+2 to Overawe as long as the target can see your face (30 feet)"
        [ "There’s something in your stare that makes others nervous. When your eye starts twitching, someone’s about to get carried to Boot Hill. Clint Eastwood has it, and so does your gunslinger."
        , "A character with “the stare” may add +2 to his overawe attacks as long as the intended victim is close enough to look into his steely gaze (usually less than 30 feet)"
        ]
    toEdgeTuple t sd ls = (EffectName t, EffectMeta sd ls)

edges
  :: (MonadHold t m, MonadFix m, PostBuild t m, DomBuilder t m)
  => Dynamic t (Set Edges)
  -> m (Event t (Endo EffectMap))
edges eDyn = elClass "div" "effects-section" $ do
  el "h2" $ text "Edges"
  let eMapDyn = edgeMap <$> eDyn
  _ <- listWithKey eMapDyn $ \k emDyn -> elClass "div" "effect" $ mdo
    elClass "span" "name" . text . unEffectName $ k
    text ": "
    elClass "span" "short-desc" . dynText . fget effectMetaDesc $ emDyn
    void . dyn $ (fget effectMetaLongDesc emDyn) <&> (traverse_ longDesc . nonEmpty)
    pure ()
  pure never
  where
    longDesc l = mdo
      openDyn <- foldDyn (const not) False toggleE
      (buttEl,_) <- elClass' "button" "toggle-desc" . text $ "expand"
      let toggleE = domEvent Click buttEl
      let lines = elClass "div" "long-desc" $ traverse_ (el "p" . text) l
      let next = bool blank lines <$> updated openDyn
      widgetHold blank next
      pure ()
