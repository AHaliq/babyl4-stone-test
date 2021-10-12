module Style
  ( sel
  , ppt
  , imp
  )where

sel :: String -> [String] -> String
sel s cs = concat $ [s, "{"] ++ cs ++ ["}"]

ppt :: String -> String -> String
ppt k v = concat [k, ":", v, ";"]

imp :: String -> String
imp url = concat ["@import url('", url, "');"]