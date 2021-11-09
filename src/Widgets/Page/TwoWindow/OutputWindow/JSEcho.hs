module Widgets.Page.TwoWindow.OutputWindow.JSEcho
  ( widget,
  )
where

import qualified Data.Text as T
import Reflex.Dom.Core

#ifdef MIN_VERSION_ghcjs_dom_jsffi
import Control.Monad.Trans (liftIO)
import qualified GHCJS.DOM.Types as GDT (pToJSVal)
import GHCJS.Types (JSVal)

foreign import javascript safe "console.log $1"
  jslog :: JSVal -> IO ()

widget :: forall t m. MonadWidget t m => Dynamic t T.Text -> m ()
widget l4ast =
  performEvent_ $ fmap liftIO $ updated $ jslog . GDT.pToJSVal . T.unpack <$> l4ast

#else
widget :: forall t m. MonadWidget t m => Dynamic t T.Text -> m ()
widget _ = return ()
#endif
