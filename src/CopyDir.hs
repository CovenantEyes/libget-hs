module CopyDir
  ( copyDir
  ) where

-- Copied from http://stackoverflow.com/questions/6807025/what-is-the-haskell-way-to-copy-a-directory

import System.Directory
import System.FilePath ((</>))
import Control.Applicative ((<$>))
import Control.Concurrent.Async (mapConcurrently)
import Control.Exception (throw)
import Control.Monad (when, void)


copyDir :: FilePath -> FilePath -> IO ()
copyDir src dst = do
  whenM (not <$> doesDirectoryExist src) $
    throw (userError "source does not exist")
  whenM (doesDirectoryExist dst `orM` doesFileExist dst) $
    throw (userError "destination already exists")

  createDirectoryIfMissing True dst
  files <- filter (`notElem` [".", ".."]) <$> getDirectoryContents src
  void $ forConcurrently files $ \name -> do
    let srcPath = src </> name
    let dstPath = dst </> name
    isDirectory <- doesDirectoryExist srcPath
    if isDirectory
      then copyDir srcPath dstPath
      else copyFile srcPath dstPath

  where
    forConcurrently = flip mapConcurrently
    whenM s r = s >>= flip when r

    orM :: Monad m => m Bool -> m Bool -> m Bool
    orM m1 m2 = m1 >>= \x-> if x then return x else m2
