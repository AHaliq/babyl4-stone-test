module Utils.String
  ( indent,
    breaki,
  )
where

import qualified Data.Bifunctor as B
import Data.Char (isSpace)
import Data.List (intercalate)

indent :: String -> String
indent [] = []
indent xs = intercalate "\n" $ map ((\(s, i) -> replicate (i * 2) ' ' ++ s) . B.first (dropWhile isSpace)) (breaki 0 xs)

breaki :: Int -> String -> [(String, Int)]
breaki i [] = [([], i)]
breaki i (x : xs)
  | x == ',' = (x : "", i) : breaki i xs
  | x `elem` ['(', '[', '{'] = (x : "", i) : breaki (i + 1) xs
  | x `elem` [')', ']', '}'] = case breaki (i - 1) xs of
    (str, i') : rs -> ("", i) : (x : str, i') : rs
    _ -> error "invalid breaki on dedent"
  | otherwise = case breaki i xs of
    (str, i') : rs -> (x : str, i') : rs
    _ -> error "invalid breaki on same indent"