module Widgets.Page.TwoWindow.OutputWindow.ASTTab
  ( widget,
  )
where

import qualified Data.Text as T
import L4.Parser (parseProgram)
import Reflex.Dom.Core
import Utils.String

widget :: (DomBuilder t m, PostBuild t m) => Dynamic t T.Text -> m ()
widget l4ast =
  elAttr "textArea" ("spellcheck" =: "false") $
    dynText $ T.pack . indent . show . parseProgram "" . T.unpack <$> l4ast