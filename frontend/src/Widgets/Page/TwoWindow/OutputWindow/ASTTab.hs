
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module Widgets.Page.TwoWindow.OutputWindow.ASTTab
  ( widget,
  )
where

import qualified Data.Text as T
import qualified Reflex.Dom.Ace as Ace
import Reflex.Dom.Core

import L4.Parser (parseProgram)
import L4.Lexer (Err)
import L4.Syntax (Program)
import L4.Annotation (SRng)
import Utils.L4D3Json
import Utils.String

-- import Control.Monad.Trans (liftIO)
-- import qualified GHCJS.DOM.Types as GDT (pToJSVal)
import GHCJS.Types (JSVal, nullRef)
import Language.Javascript.JSaddle
import Control.Monad (void)

default (T.Text)

-- foreign import javascript safe "d3render($1, $2, document.querySelector('.d3div'));"
--   d3render :: JSVal -> JSVal -> IO ()

-- :: MonadJSM m => String -> m ()
d3render :: MonadJSM m => String -> JSVal -> String -> m ()
-- d3render _ _ = _ "d3render($1, $2, document.querySelector('.d3div'));"
d3render a b c = void . liftJSM $ jsgf "d3renderOnEleAddToNode"  (a, b, c)

widget :: forall t m. MonadWidget t m => Dynamic t (T.Text, Maybe Ace.AceInstance) -> m ()
widget l4ast = do
  widgetHold_ (return ()) $ updated $ widgetOfL4 . (\(a,b) -> (parseProgram "" . T.unpack $ a, maybe nullRef Ace.unAceInstance $ b)) <$> l4ast
  return ()

widgetOfL4 :: forall t m. MonadWidget t m => (Either Err (Program SRng), JSVal) -> m ()
widgetOfL4 (Left x, _) = do
  elAttr "textArea" ("spellcheck" =: "false") $ text $ T.pack . indent . show $ x
  return ()
widgetOfL4 (Right x, jseditor) = do
  elClass "div" "d3div" $ return ()
  d3render "d3div" jseditor $ d3json x
  return ()
  