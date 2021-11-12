{-# LANGUAGE MonoLocalBinds #-}

module Widgets.Page.TwoWindow
  ( widget,
  )
where

import Reflex.Dom.Core
import qualified Widgets.Common.Editor as E
import qualified Widgets.Page.TwoWindow.OutputWindow as OutputWindow

widget ::
  MonadWidget t m =>
  m ()
widget = do
  el "title" $ text "Try L4"
  elAttr "meta" ("name" =: "viewport" <> "content" =: "width=device-width, initial-scale=1") $ return ()
  _ <- elAttr' "script" ("src" =: "js/theme.js") $ return ()
  _ <- elAttr' "script" ("src" =: "https://d3js.org/d3.v7.min.js") $ return ()
  _ <- elAttr' "script" ("src" =: "js/d3.js") $ return ()
  elClass "div" "container" $ do
    el "h1" $ text "L4"
    elClass "div" "content" $ do
      t <- E.widget
      OutputWindow.widget t