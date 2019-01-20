{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Frontend.Style where

import           Prelude                  hiding (rem)
import           Clay
import qualified Clay.Flexbox             as FB
import           Data.List.NonEmpty       (NonEmpty ((:|)))
import           Data.Semigroup.Foldable  (fold1)

import           Obelisk.Generated.Static as Static

card :: Css
card = do
  backgroundColor (rgb 246 246 246)
  border dotted (px 1) (rgb 153 153 153)

style :: Css
style = do
  fontFace $ do
    fontFamily ["SecretST"] []
    fontWeight normal
    fontFaceSrc
      [ FontFaceSrcUrl (Static.static @"secretst-webfont.woff2") (Just WOFF2)
      , FontFaceSrcUrl (Static.static @"secretst-webfont.woff")  (Just WOFF)
      ]
  fontFace $ do
    fontFamily ["Durango"] []
    fontWeight normal
    fontFaceSrc
      [ FontFaceSrcUrl (Static.static @"durango_western_eroded_demo.woff2") (Just WOFF2)
      , FontFaceSrcUrl (Static.static @"durango_western_eroded_demo.woff")  (Just WOFF)
      ]

  fold1 (h1 :| [h2,h3,h4]) ? do
    fontFamily ["Durango"] []
    fontWeight normal
    color (rgb 48 48 48)
    lineHeight (em 1.2)
    textDecoration underline

  h1 ? fontSizeCustom xLarge
  h2 ? fontSizeCustom larger
  h3 ? fontSizeCustom large

  fold1 (html :| [body, "#app"]) ? do
    fontSize (em 1)

  body ? do
    backgroundImage . url $ Static.static @"paper.png"
    color (rgb 48 48 48)
    fontFamily ["SecretST"] [monospace]

  ".character-sheet" ? do
    display flex
    flexDirection row
    justifyContent spaceAround
    width (pct 100)
    height (pct 100)

    ".effects" ? do
      FB.flex 0 0 (px 400)
      textAlign center

      ".info" ? do
        card
        overflow auto
        textAlign (alignSide sideLeft)
        padding (rem 0) (rem 2) (rem 1) (rem 2)
        h2 ? textAlign (alignSide sideCenter)
        img ? do
          marginLeft (rem 1)
          float floatRight
          border solid (px 2) (rgb 48 48 48)
        dl ? do
          dt ? do
            marginTop (em 0.5)
            fontWeight bold
          dd ? do
            "margin-inline-start" -: "0.5em"
        marginBottom (rem 3)

  pure ()
