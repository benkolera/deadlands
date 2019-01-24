{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Frontend where

import           Control.Lens

import           Clay                     (render)
import qualified Data.Map                 as Map
import qualified Data.Text.Lazy           as TL
import           Data.Text.Lens           (packed)
import           Obelisk.Frontend
import           Obelisk.Route
import           Reflex.Dom

import           Common.CharacterSheet
import           Common.Route
import           Frontend.Internal        (fget)
import           Frontend.Style
import           Frontend.Traits          (traits)
import           Obelisk.Generated.Static

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
