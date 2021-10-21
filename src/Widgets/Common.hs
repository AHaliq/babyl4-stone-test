module Widgets.Common
  ( dynButtonClass,
    buttonClass,
  )
where

import qualified Data.Text as T
import Reflex.Dom.Core

dynButtonClass ::
  ( DomBuilder t m,
    PostBuild t m
  ) =>
  Dynamic t T.Text ->
  T.Text ->
  m (Event t ())
dynButtonClass c s = do
  (e, _) <- elDynClass' "div" c $ text s
  return $ domEvent Click e

buttonClass :: DomBuilder t m => T.Text -> T.Text -> m (Event t ())
buttonClass c s = do
  (e, _) <- elAttr' "div" ("class" =: c) $ text s
  return $ domEvent Click e