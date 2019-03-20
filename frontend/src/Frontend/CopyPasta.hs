{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Frontend.CopyPasta where

import           Control.Lens

import           Data.Bool                  (bool)
import qualified Data.Map                   as Map
import           Data.Number.Nat1           (Nat1)
import qualified GHCJS.DOM.Document         as JsDocument
import qualified GHCJS.DOM.HTMLInputElement as JsInput
import           GHCJS.DOM.Types            (MonadJSM)
import           Reflex.Dom

import           Common.DiceSet
import           Obelisk.Generated.Static

copyPasta
  :: forall t m js
  . ( DomBuilder t m, MonadSample t m, PerformEvent t m, HasDocument m
    , Prerender js m, MonadJSM (Performable m)
    )
  => Bool
  -> Dynamic t Nat1
  -> Dynamic t DiceSet
  -> m ()
copyPasta inline tnDyn dsDyn =  do
  let dcDyn = (\tn -> toFgCode (SetVsTn (TnCheck tn True))) <$> tnDyn <*> dsDyn
  dcInit <- sample . current $ dcDyn
  ti <- inputElement $ ( def :: InputElementConfig EventResult t (DomBuilderSpace m))
    & inputElementConfig_initialValue .~ dcInit
    & inputElementConfig_setValue     .~ updated dcDyn
    & inputElementConfig_elementConfig.elementConfig_initialAttributes .~
      ("class" =: ("copypasta " <> bool "hidden" "inline" inline))
  (imgElt,_) <- elAttr' "img"
    (Map.fromList
      [("src",bool (static @"dice.svg") (static @"copy-content.svg") inline)
       ,("class", bool "dice-icon" "copy-icon" inline)
      ])
      blank
  prerender blank $ do
    let copyE = True <$ domEvent Click imgElt
    let rawElt = ti ^. to _inputElement_raw
    doc <- askDocument
    performEvent_ $ copyToClipboard rawElt doc <$ copyE
  where
    copyToClipboard elt doc = do
      JsInput.select elt
      -- Watch out, this can only be executed in a user event so it wont work
      -- in ob run. Sadness.
      JsDocument.execCommand_ doc ("copy"::String) False (Nothing::Maybe String)
      pure ()
