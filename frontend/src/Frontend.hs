{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Frontend where

import qualified Data.Text as T
import Reflex.Dom.Core

import Common.CharacterSheet
import Static

frontend :: (StaticWidget x (), Widget x ())
frontend = (head', body)
  where
    head' = el "title" $ text "Obelisk Minimal Example"
    body = do
      text "asss"
      elAttr "img" ("src" =: static @"obelisk.jpg") blank
