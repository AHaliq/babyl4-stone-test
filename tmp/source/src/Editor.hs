{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Editor
  ( widget,
  )
where

import Control.Monad (void)
import Data.Functor (($>))
import Data.Text (Text)
import qualified Language.Javascript.JSaddle.Types as JS
import qualified Reflex.Dom.Ace as Ace
import Reflex.Dom.Core ((=:))
import qualified Reflex.Dom.Core as R

widget ::
  forall t m.
  ( R.DomBuilder t m,
    R.TriggerEvent t m,
    JS.MonadJSM (R.Performable m),
    JS.MonadJSM m,
    R.PerformEvent t m,
    R.PostBuild t m,
    R.MonadHold t m
  ) =>
  m (R.Dynamic t Text)
widget = do
  let containerId = "editor"
  void $
    R.elAttr
      "div"
      ( "id" =: containerId
      )
      R.blank
  (script, _) <-
    R.elAttr'
      "script"
      ( "src" =: "https://cdnjs.cloudflare.com/ajax/libs/ace/1.4.12/ace.js"
          <> "type" =: "text/javascript"
          <> "charset" =: "utf-8"
      )
      R.blank
  let scriptLoaded = () <$ R.domEvent R.Load script
  let loading = R.el "p" $ R.text "Loading editor..." $> R.constDyn ""
  dt :: R.Dynamic t (R.Dynamic t Text) <- R.widgetHold loading $
    R.ffor scriptLoaded $
      const $ do
        ace <- do
          let cfg =
                R.def
                  { Ace._aceConfigMode = Just "haskell"
                  }
          Ace.aceWidget cfg (Ace.AceDynConfig (Just Ace.AceTheme_PastelOnDark)) R.never containerId "" R.never
        return $ Ace.aceValue ace
  R.holdDyn "" . R.switchDyn $ R.updated <$> dt