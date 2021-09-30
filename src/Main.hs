{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Monad.Fix (MonadFix)
import Control.Monad (void)
import qualified Language.Javascript.JSaddle.Types as JS
import qualified Reflex.Dom.Core as R
import qualified Data.Text as T
import Reflex.Dom

import L4.Parser (parseProgram)
import Helpers (css, static)
import StringUtils (indent)
import qualified Editor as E

main :: IO ()
main =
  mainWidgetWithHead head2 body2

head2 :: DomBuilder t m => m ()
head2 = do
  el "title" $ text "Try L4"
  css $ static "main.css"


body2 ::
  ( R.DomBuilder t m
  , R.TriggerEvent t m
  , JS.MonadJSM (R.Performable m)
  , JS.MonadJSM m
  , R.PerformEvent t m
  , R.PostBuild t m
  , R.MonadHold t m
  ) =>
  m ()
body2 = do
  elClass "div" "container" $ do
    el "h1" $ text "L4"
    elClass "div" "content" $ do
      t :: Dynamic t T.Text <- E.widget
      elAttr "textArea" ("spellcheck" =: "false") $ parseDyn t
  return ()

parseDyn :: (PostBuild t m, DomBuilder t m) => Dynamic t T.Text -> m ()
parseDyn t = dynText $ T.pack . indent . show . parseProgram "" . T.unpack <$> t

headWidget :: DomBuilder t m => m ()
headWidget = do
  elAttr "meta" ("http-equiv" =: "Content-Type" <> "content" =: "text/html; charset=utf-8") blank
  elAttr "meta" ("name" =: "viewport" <> "content" =: "width=device-width, initial-scale=1") blank
  el "title" $ text "Try L4"

bodyWidget ::
  ( DomBuilder t m,
    MonadFix m,
    MonadHold t m,
    PostBuild t m
  ) =>
  m ()
bodyWidget = do
  el "h1" $ text "reflex-stone"
  clicked <- stoneButton
  cnt <- foldDyn (+) (0 :: Int) $ 1 <$ clicked
  elClass "p" "result" $ do
    dyn_ $
      ffor cnt $ \case
        0 -> text "Go ahead and hit the stone"
        n -> do
          text $ T.pack (show n)
          text " heads!"
  divClass "footer" $ do
    elAttr "a" ("href" =: homePage) $
      text "View source on GitHub"
  where
    homePage = "https://github.com/srid/reflex-stone"

stoneButton :: DomBuilder t m => m (Event t ())
stoneButton = do
  let attr = "style" =: "font-size: 200%;"
  clickEvent $ elAttr' "button" attr stone

stone :: DomBuilder t m => m ()
stone =
  text "🗿"

-- | Get the click event on an element
--
-- Use as:
--   clickEvent $ el' "a" ...
clickEvent ::
  ( DomBuilder t m,
    HasDomEvent t target 'ClickTag
  ) =>
  m (target, a) ->
  m (Event t ())
clickEvent = fmap $ void . domEvent Click . fst
