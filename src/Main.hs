{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Data.String (fromString)
import qualified Language.Javascript.JSaddle.Types as JS
import qualified Reflex.Dom.Core as R
import qualified Data.Text as T
import Reflex.Dom

import L4.Parser (parseProgram)
import StringUtils (indent)
import Style
import qualified Editor as E

main :: IO ()
main =
  let cssString = fromString style in
  mainWidgetWithCss cssString $ do
    headWidget
    bodyWidget

style :: String
style = concat [
  imp "https://fonts.googleapis.com/css2?family=Poppins&family=Roboto+Mono&family=Slabo+27px&display=swap",
  sel ":root" [
    ppt "--bg-col" "#2c2828",
    ppt "--fg-col" "#8f938f",
    ppt "--fg2-col" "#545558"
  ],
  sel "h1" [
    ppt "font-family" "'Slabo 27px', serif",
    ppt "font-size" "3rem"
  ],
  sel "html" [
    ppt "height" "100%",
    ppt "background-color" "var(--bg-col)",
    ppt "color" "var(--fg-col)",
    ppt "font-size" "12px"
  ],
  sel "body" [
    ppt "padding" "0",
    ppt "margin" "0",
    ppt "height" "100%",
    ppt "display" "flex"
  ],
  sel ".container" [
    ppt "display" "flex",
    ppt "flex-direction" "column",
    ppt "flex-grow" "1",
    ppt "margin" "1rem"
  ],
  sel ".content" [
    ppt "display" "grid",
    ppt "grid-template-columns" "1fr 1fr",
    ppt "flex-grow" "1"
  ],
  sel ".section" [ppt "font-family" "'Poppins', sans-serif"],
  sel "p" [ppt "color" "red"],
  sel "textarea" [
    ppt "resize" "none",
    ppt "font-family" "'Roboto Mono', monospace",
    ppt "border" "none",
    ppt "background-color" "inherit",
    ppt "color" "inherit",
    ppt "padding-left" "1rem",
    ppt "outline" "none"
  ],
  sel "::selection" [ppt "background-color" "var(--fg2-col)"],
  sel "@media only screen and (max-width: 600px)" [
    sel ".content" [
      ppt "grid-template-columns" "1fr",
      ppt "grid-template-rows" "1fr 1fr"
    ]
  ]
  ]

headWidget :: DomBuilder t m => m ()
headWidget = do
  el "title" $ text "Try L4"


bodyWidget ::
  ( R.DomBuilder t m
  , R.TriggerEvent t m
  , JS.MonadJSM (R.Performable m)
  , JS.MonadJSM m
  , R.PerformEvent t m
  , R.PostBuild t m
  , R.MonadHold t m
  ) =>
  m ()
bodyWidget = do
  elClass "div" "container" $ do
    el "h1" $ text "L4"
    elClass "div" "content" $ do
      t :: Dynamic t T.Text <- E.widget
      elAttr "textArea" ("spellcheck" =: "false") $ parseDyn t
  return ()

parseDyn :: (PostBuild t m, DomBuilder t m) => Dynamic t T.Text -> m ()
parseDyn t = dynText $ T.pack . indent . show . parseProgram "" . T.unpack <$> t