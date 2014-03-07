module Utils
  ( jsonField
  ) where

import Data.Char


jsonField :: String -> String
jsonField = map toLower . dropWhile (not . isUpper)
