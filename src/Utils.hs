module Utils
  ( jsonField
  , printError
  ) where

import Data.Char
import System.IO (stderr, hPutStrLn)


jsonField :: String -> String
jsonField = map toLower . dropWhile (not . isUpper)


printError :: String -> IO ()
printError msg = hPutStrLn stderr $ "Error: " ++ msg
