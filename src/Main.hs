{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Monad.Fix (MonadFix)
import Data.Functor ((<&>))
import Data.String (fromString)
import qualified Data.Text as T
import qualified Editor as E
import L4.Parser (parseProgram)
import qualified Language.Javascript.JSaddle.Types as JS
import Reflex.Dom
import qualified Reflex.Dom.Core as R
import Reflex.Dynamic (holdDyn)
import StringUtils (indent)
import Style

main :: IO ()
main =
  let cssString = fromString style
   in mainWidgetWithCss cssString $ do
        headWidget
        bodyWidget

style :: String
style =
  concat
    [ imp "https://fonts.googleapis.com/css2?family=Poppins&family=Roboto+Mono&family=Slabo+27px&display=swap",
      sel
        ":root"
        [ ppt "--bg-col" "#2c2828",
          ppt "--fg-col" "#8f938f",
          ppt "--fg2-col" "#545558"
        ],
      sel
        "h1"
        [ ppt "font-family" "'Slabo 27px', serif",
          ppt "font-size" "3rem"
        ],
      sel
        "html"
        [ ppt "height" "100%",
          ppt "background-color" "var(--bg-col)",
          ppt "color" "var(--fg-col)",
          ppt "font-size" "12px"
        ],
      sel
        "body"
        [ ppt "padding" "0",
          ppt "margin" "0",
          ppt "height" "100%",
          ppt "display" "flex"
        ],
      sel
        ".container"
        [ ppt "display" "flex",
          ppt "flex-direction" "column",
          ppt "flex-grow" "1",
          ppt "margin" "1rem"
        ],
      sel
        ".content"
        [ ppt "display" "grid",
          ppt "grid-template-columns" "1fr 1fr",
          ppt "flex-grow" "1"
        ],
      sel
        ".tabwindow"
        [ ppt "display" "flex",
          ppt "flex-direction" "column",
          ppt "flex-grow" "1"
        ],
      sel
        ".tablist"
        [ ppt "display" "flex",
          ppt "flex-wrap" "nowrap",
          ppt "justify-content" "flex-start",
          ppt "gap" "1rem"
        ],
      sel
        ".tabbtn"
        [ ppt "user-select" "none",
          ppt "cursor" "pointer",
          ppt "font-family" "'Roboto Mono', monospace",
          ppt "letter-spacing" "0.1rem",
          ppt "color" "var(--fg2-col)",
          ppt "background-color" "var(--bg-col)",
          ppt "border-bottom" "0.2rem solid var(--fg2-col)"
        ],
      sel
        ".tabselected"
        [ ppt "color" "var(--fg-col)",
          ppt "border-bottom" "0.2rem solid var(--fg-col)"
        ],
      sel ".section" [ppt "font-family" "'Poppins', sans-serif"],
      sel "p" [ppt "color" "red"],
      sel
        "textarea"
        [ ppt "resize" "none",
          ppt "flex-grow" "1",
          ppt "font-family" "'Roboto Mono', monospace",
          ppt "border" "none",
          ppt "background-color" "inherit",
          ppt "color" "inherit",
          ppt "padding-left" "1rem",
          ppt "outline" "none"
        ],
      sel "::selection" [ppt "background-color" "var(--fg2-col)"],
      sel
        "@media only screen and (max-width: 600px)"
        [ sel
            ".content"
            [ ppt "grid-template-columns" "1fr",
              ppt "grid-template-rows" "1fr 1fr"
            ]
        ]
    ]

headWidget :: DomBuilder t m => m ()
headWidget = do
  el "title" $ text "Try L4"
  elAttr "meta" ("name" =: "viewport" <> "content" =: "width=device-width, initial-scale=1") $ return ()

bodyWidget ::
  ( R.DomBuilder t m,
    R.TriggerEvent t m,
    JS.MonadJSM (R.Performable m),
    JS.MonadJSM m,
    R.PerformEvent t m,
    R.PostBuild t m,
    R.MonadHold t m,
    MonadFix m
  ) =>
  m ()
bodyWidget = do
  elClass "div" "container" $ do
    el "h1" $ text "L4"
    elClass "div" "content" $ do
      t <- E.widget
      elClass "div" "tabwindow" $ do
        eos <- tabWidget Hello [Hello, There, Welcome, To, L4]
        widgetHold_ (el "div" $ text "click a button") $
          eos
            <&> \case
              L4 -> elAttr "textArea" ("spellcheck" =: "false") $ parseDyn t
              b -> el "h1" $ text . T.pack . show $ b

parseDyn :: (R.PostBuild t m, R.DomBuilder t m) => Dynamic t T.Text -> m ()
parseDyn t = dynText $ T.pack . indent . show . parseProgram "" . T.unpack <$> t

data TestTabs = Hello | There | Welcome | To | L4
  deriving (Show, Eq, Ord)

tabWidget ::
  ( R.DomBuilder t m,
    R.MonadHold t m,
    R.PostBuild t m,
    MonadFix m,
    Eq a,
    Show a
  ) =>
  a ->
  [a] ->
  m (Event t a)
tabWidget ix xs = do
  elClass "div" "tablist" $ mdo
    bs <- leftmost <$> mapM (btn d) ixs
    d <- holdDyn ix bs
    return bs
  where
    ixs = map (\x -> (x, T.toLower . T.pack . show $ x)) xs
    btn d (i, x) =
      (i <$)
        <$> dynButtonClass
          ((\j -> T.concat ["tabbtn", if i == j then " tabselected" else ""]) <$> d)
          x

dynButtonClass ::
  ( R.DomBuilder t m,
    R.PostBuild t m
  ) =>
  Dynamic t T.Text ->
  T.Text ->
  m (Event t ())
dynButtonClass c s = do
  (e, _) <- elDynClass' "div" c $ text s
  return $ domEvent Click e

{-
buttonClass :: DomBuilder t m => T.Text -> T.Text -> m (Event t ())
buttonClass c s = do
  (e, _) <- elAttr' "div" ("class" =: c) $ text s
  return $ domEvent Click e
-}