module Widgets.Page.TwoWindow.OutputWindow
  ( Tabs,
    widget,
  )
where

import Control.Monad.Trans (liftIO)
import Data.Functor ((<&>))
import qualified Data.Text as T
import qualified GHCJS.DOM.Types as GDT (pToJSVal)
import GHCJS.Types (JSVal)
import Reflex.Dom.Core
import qualified Widgets.Common.TabbedWindow as TabbedWindow
import qualified Widgets.Page.TwoWindow.OutputWindow.ASTTab as ASTTab

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

foreign import javascript safe "console.log $1"
  jslog :: JSVal -> IO ()

widgetOfTab :: forall t m. MonadWidget t m => Dynamic t T.Text -> Tabs -> m ()
widgetOfTab l4ast L4 = ASTTab.widget l4ast
widgetOfTab l4ast To = do
  let jsv :: Dynamic t (IO ())
      jsv = jslog . GDT.pToJSVal <$> l4ast
      jsvIOEvent :: Event t (IO ())
      jsvIOEvent = updated jsv
      jsvHostEvent :: Event t (WidgetHost m ())
      jsvHostEvent = fmap liftIO jsvIOEvent
   in performEvent_ jsvHostEvent
  el "h1" $ text . T.pack $ "To"
widgetOfTab _ b = el "h1" $ text . T.pack . show $ b