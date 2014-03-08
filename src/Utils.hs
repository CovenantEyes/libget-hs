module Utils
  ( jsonField
  , first
  , printError
  ) where

import Data.Char
import Data.Functor ((<$>))
import Data.Maybe (catMaybes)
import System.IO (stderr, hPutStrLn)


jsonField :: String -> String
jsonField = map toLower . dropWhile (not . isUpper)


-- IMPROVE: This is too strict. It runs `f` on all `as` before getting the head.
first :: (Monad m, Functor m) => (a -> m (Maybe b)) -> [a] -> m (Maybe b)
first f as = (safeHead . catMaybes) <$> mapM f as
  where safeHead [] = Nothing
        safeHead xs = Just (head xs)


printError :: String -> IO ()
printError msg = hPutStrLn stderr $ "Error: " ++ msg