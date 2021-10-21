{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Widgets.Common.TabbedWindow
  ( widget,
  )
where

import Control.Monad.Fix (MonadFix)
import qualified Data.Text as T
import Reflex.Dom.Core

import Widgets.Common 

widget ::
  ( DomBuilder t m,
    MonadHold t m,
    PostBuild t m,
    MonadFix m,
    Eq a,
    Show a
  ) =>
  a ->
  [a] ->
  m (Event t a)
widget ix xs = do
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