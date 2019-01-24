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

  let headerFont = fontFamily ["Durango"] []
  let bodyFont   = fontFamily ["SecretST"] [monospace]

  fold1 (h1 :| [h2,h3,h4]) ? do
    headerFont
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
    bodyFont

  ".dice-icon" ? do
    width (em 1.2)
    height (em 1.2)

  ".copy-icon" ? do
    width (em 1.2)
    height (em 1.2)

  ".dicecode-container" ? do
    textAlign (alignSide sideLeft)
    display inline
    paddingLeft (em 0.2)

    ".copy-icon" ? do
      marginBottom (em (- 0.2))

    ".dice-codes" ? do
      marginTop (em 1)
      marginBottom (em 1)
      width (em 20)

    ".dice-code" ? do
      display flex
      marginTop (em 0.5)
      ".dice-code-label" ? do
        flexGrow 1
      ".dice-code-code" ? do
        padding (em 0) (em 0.5) (em 0) (em 0.5)

  ".copypasta" ? do
    ".inline" & do
      bodyFont
      fontSize (em 1)
      backgroundColor (rgba 255 255 255 255)
      borderWidth (em 0)
      padding (em 0) (em 0.5) (em 0) (em 0.5)
      width (em 10)
      textAlign (alignSide sideRight)
      ":focus" &
        outlineWidth (em 0)
    ".hidden" & do
      position absolute
      top (px (-666))
      left (px (-666))

  ".modal" ? do
    display none
    position fixed
    zIndex 1
    left (px 0)
    top (px 0)
    width (pct 100)
    height (pct 100)
    overflow auto
    backgroundColor (rgba 0 0 0 0.4)

    ".opened" & do
      display block

    ".modal-content" ? do
      backgroundColor (rgb 254 254 254)
      margin (pct 15) auto (pct 15) auto
      padding (em 2) (em 2) (em 2) (em 2)
      border solid (px 1) (rgb 136 136 136)
      width (pct 80)

  ".character-sheet" ? do
    display flex
    flexDirection row
    justifyContent spaceAround
    textAlign (alignSide sideCenter)
    width (pct 100)
    height (pct 100)

    ".traits" ? do
      flexGrow 1
      marginRight (rem 3)
      marginLeft (rem 0.5)
      ".trait" ? do
        display flex
        textAlign (alignSide sideLeft)
        flexWrap FB.wrap
        flexDirection row
        "column-count" -: "2"
        width (rem 20)
        listStyleType none
        marginBottom (rem 1)
        marginLeft (em (-1))

        ".trait-name" ? do
          flexGrow 1
        ".trait-dice" ? do
          FB.flex 0 0 (rem 6)
          textAlign (alignSide sideRight)
        ".aptitudes" ? do
          listStyleType none
          marginLeft (em (-1))
          width (rem 19)
          ".concentrations" ? do
            listStyleType none
            marginLeft (em (-1))
            width (rem 17)
          ".aptitude" ? do
            display flex
            ".name" ? do
              flexGrow 1
            ".value" ? do
              FB.flex 0 0 (em 5)
              textAlign (alignSide sideRight)

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

    ".spells" ? do
      flexGrow 2
      textAlign center
      marginLeft (em 3)
      marginRight (em 0.5)
      height (pct 100)
      overflow auto
      display block

  pure ()
