module Widgets.Page.TwoWindow
  ( widget,
  )
where

import Control.Monad.Fix (MonadFix)
import Language.Javascript.JSaddle.Types
import Reflex.Dom.Core
import qualified Widgets.Common.Editor as E
import qualified Widgets.Page.TwoWindow.OutputWindow as OutputWindow

widget ::
  ( DomBuilder t m,
    TriggerEvent t m,
    MonadJSM (Performable m),
    MonadJSM m,
    PerformEvent t m,
    PostBuild t m,
    MonadHold t m,
    MonadFix m
  ) =>
  m ()
widget = do
  el "title" $ text "Try L4"
  elAttr "meta" ("name" =: "viewport" <> "content" =: "width=device-width, initial-scale=1") $ return ()
  elClass "div" "container" $ do
    el "h1" $ text "L4"
    elClass "div" "content" $ do
      t <- E.widget
      OutputWindow.widget t