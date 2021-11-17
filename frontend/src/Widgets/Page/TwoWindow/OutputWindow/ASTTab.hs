
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
d3render :: MonadJSM m => JSVal -> String -> m ()
-- d3render _ _ = _ "d3render($1, $2, document.querySelector('.d3div'));"
d3render a b = void . liftJSM $ jsgf "d3render"  (a, b, eval "document.querySelector('.d3div')")

widget :: forall t m. MonadWidget t m => Dynamic t (T.Text, Maybe Ace.AceInstance) -> m ()
widget l4ast = do
  widgetHold_ (return ()) $ updated $ widgetOfL4 <$> parsed
  elClass "div" "d3div" $ return ()
  performEvent_ $ updated $ d3render <$> jseditor <*> parsed'
  return ()
  where
    jseditor = maybe nullRef Ace.unAceInstance . snd <$> l4ast
    parsed = parseProgram "" . T.unpack . fst <$> l4ast
    parsed' = either (const "") (show . d3json) <$> parsed

widgetOfL4 :: forall t m. MonadWidget t m => Either Err (Program SRng) -> m ()
widgetOfL4 (Left x) = do
  elAttr "textArea" ("spellcheck" =: "false") $ text $ T.pack . indent . show $ x
  return ()
widgetOfL4 (Right _) = do
  return ()

  -- either f g $ parseProgram "" . T.unpack <$> l4ast
  -- where
  --   f x = elAttr "textArea" ("spellcheck" =: "false") $
  --     dynText $ T.pack . indent . show <$> x
  --   g x = do
  --     elClass "div" "d3div" $ return ()
  --     performEvent_ $ fmap liftIO $ updated $ d3render . GDT.pToJSVal . show . d3json <$> x