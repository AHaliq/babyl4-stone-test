{-# LANGUAGE MonoLocalBinds #-}
module Widgets.Page.TwoWindow.OutputWindow
  ( Tabs,
    widget,
  )
where

import Data.Functor ((<&>))
import qualified Data.Text as T
import Reflex.Dom.Core
import qualified Widgets.Common.TabbedWindow as TabbedWindow
import qualified Widgets.Page.TwoWindow.OutputWindow.ASTTab as ASTTab
import qualified Widgets.Page.TwoWindow.OutputWindow.JSEcho as JSEcho

data Tabs = Hello | There | Welcome | To | L4
  deriving (Show, Eq)

widget ::
  MonadWidget t m =>
  Dynamic t T.Text ->
  m ()
widget l4ast = do
  elClass "div" "tabwindow" $ do
    tabEvents <- TabbedWindow.widget Hello [Hello, There, Welcome, To, L4]
    widgetHold_ (el "div" $ text "click a button") $ tabEvents <&> widgetOfTab l4ast
    return ()

widgetOfTab :: forall t m. MonadWidget t m => Dynamic t T.Text -> Tabs -> m ()
widgetOfTab l4ast L4 = ASTTab.widget l4ast
widgetOfTab l4ast To = do
  JSEcho.widget l4ast
  el "h1" $ text . T.pack $ "To"
widgetOfTab _ b = el "h1" $ text . T.pack . show $ b