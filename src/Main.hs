{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Data.String (fromString)
import Reflex.Dom
import qualified Widgets.Page.TwoWindow as W
import Widgets.Page.TwoWindow.Style (css)

main :: IO ()
main =
  let cssString = fromString css
   in mainWidgetWithCss cssString W.widget