module Widgets.Page.TwoWindow.OutputWindow
  ( Tabs,
    widget,
  )
where

import Control.Monad.Fix (MonadFix)
import Data.Functor ((<&>))
import qualified Data.Text as T
import L4.Parser (parseProgram)
import Reflex.Dom.Core
import Utils.String
import qualified Widgets.Common.TabbedWindow as TabbedWindow

data Tabs = Hello | There | Welcome | To | L4
  deriving (Show, Eq)

widget ::
  ( PostBuild t m,
    DomBuilder t m,
    MonadFix m,
    MonadHold t m
  ) =>
  Dynamic t T.Text ->
  m ()
widget t = do
  elClass "div" "tabwindow" $ do
    eos <- TabbedWindow.widget Hello [Hello, There, Welcome, To, L4]
    widgetHold_ (el "div" $ text "click a button") $
      eos
        <&> \case
          L4 ->
            let f d = dynText $ T.pack . indent . show . parseProgram "" . T.unpack <$> d
             in elAttr "textArea" ("spellcheck" =: "false") $ f t
          b -> el "h1" $ text . T.pack . show $ b
    return ()