{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
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
import Language.Javascript.JSaddle
import Control.Monad (void)

jsaddlePrint :: MonadJSM m => String -> m ()
-- jsaddlePrint s = void $ liftJSM $ eval $ "console.log(" <> show s <> ")"
jsaddlePrint s = void $ liftJSM $ jsg "console" # "log" $ T.pack s

widget :: forall t m. MonadWidget t m => Dynamic t T.Text -> m ()
-- widget _ = return ()
widget l4ast =
  performEvent_ $ updated $ jsaddlePrint . T.unpack <$> l4ast
#endif
