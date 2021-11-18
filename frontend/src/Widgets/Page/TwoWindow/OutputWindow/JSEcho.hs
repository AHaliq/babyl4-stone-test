{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module Widgets.Page.TwoWindow.OutputWindow.JSEcho
  ( widget,
  )
where

import qualified Data.Text as T
import Reflex.Dom.Core

import Language.Javascript.JSaddle
import Control.Monad (void)

-- foreign import javascript safe "console.log $1"
--   jslog :: JSVal -> IO ()

jsaddlePrint :: MonadJSM m => String -> m ()
-- jsaddlePrint s = void $ liftJSM $ eval $ "console.log(" <> show s <> ")"
jsaddlePrint s = void $ liftJSM $ jsg "console" # "log" $ T.pack s

widget :: forall t m. MonadWidget t m => Dynamic t T.Text -> m ()
-- widget _ = return ()
widget l4ast =
  performEvent_ $ updated $ jsaddlePrint . T.unpack <$> l4ast
