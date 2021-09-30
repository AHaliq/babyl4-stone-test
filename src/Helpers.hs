
{-# LANGUAGE OverloadedStrings #-}
module Helpers
  ( css
  , script
  , static
  )where

import Reflex.Dom.Core
import Data.Text as T

css :: DomBuilder t m => Text -> m ()
css src = elAttr "link" ("href" =: src <> "type" =: "text/css" <> "rel" =: "stylesheet") blank

script :: DomBuilder t m => Text -> m ()
script src = elAttr "script" ("type" =: "text/javascript" <> "src" =: src) blank

static :: Text -> Text
static x = T.concat ["./static/", x]