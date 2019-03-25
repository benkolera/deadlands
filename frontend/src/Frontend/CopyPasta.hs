{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Frontend.CopyPasta where

import           Control.Lens

import           Control.Monad              (when)
import           Data.Bool                  (bool)
import qualified Data.Map                   as Map
import           Data.Number.Nat1           (Nat1)
import           Data.Text                  (Text)
import           Language.Javascript.JSaddle (liftJSM,jsg, JSM, js1,js)
import           Reflex.Dom

import           Common.DiceSet
import           Obelisk.Generated.Static

copyPasta
  :: forall t m js
  . ( DomBuilder t m, PerformEvent t m, Prerender js m , PostBuild t m)
  => Bool
  -> Dynamic t Nat1
  -> Dynamic t DiceSet
  -> m ()
copyPasta inline tnDyn dsDyn =  do
  let dcDyn = (\tn -> toFgCode (SetVsTn (TnCheck tn True))) <$> tnDyn <*> dsDyn
  when inline $ elClass "span" "copypasta inline" $ dynText dcDyn
  (imgElt,_) <- elAttr' "img"
    (Map.fromList
      [("src",bool (static @"dice.svg") (static @"copy-content.svg") inline)
       ,("class", bool "dice-icon" "copy-icon" inline)
      ])
      blank
  prerender blank $ do
    let copyE = True <$ domEvent Click imgElt
    performEvent_ $ liftJSM . copyToClipboard <$> current dcDyn <@ copyE
  where
    copyToClipboard :: Text -> JSM ()
    copyToClipboard t = do
      -- We should probably add this to GHCJs DOM!
      -- It still wont work on firefox yet. 
      _ <- jsg ("navigator"::String) ^. js ("clipboard"::String) . js1 ("writeText"::String) t 
      pure ()
