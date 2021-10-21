
module Widgets.Page.TwoWindow.OutputWindow.JSEcho
    ( widget ,
    )
where

import Control.Monad.Trans (liftIO)
import qualified GHCJS.DOM.Types as GDT (pToJSVal)
import GHCJS.Types (JSVal)
import qualified Data.Text as T
import Reflex.Dom.Core

foreign import javascript safe "console.log $1"
  jslog :: JSVal -> IO ()

widget :: forall t m. MonadWidget t m => Dynamic t T.Text -> m ()
widget l4ast =
  let jsv :: Dynamic t (IO ())
      jsv = jslog . GDT.pToJSVal <$> l4ast
      jsvIOEvent :: Event t (IO ())
      jsvIOEvent = updated jsv
      jsvHostEvent :: Event t (WidgetHost m ())
      jsvHostEvent = fmap liftIO jsvIOEvent
   in performEvent_ jsvHostEvent