module CopyDir
  ( copyDir
  ) where

-- Copied from http://stackoverflow.com/questions/6807025/what-is-the-haskell-way-to-copy-a-directory

import System.Directory
import System.FilePath ((</>))
import Control.Applicative ((<$>))
import Control.Exception (throw)
import Control.Monad (when, forM_)


copyDir ::  FilePath -> FilePath -> IO ()
copyDir src dst = do
  whenM (not <$> doesDirectoryExist src) $
    throw (userError "source does not exist")
  whenM (doesFileOrDirectoryExist dst) $
    throw (userError "destination already exists")

  createDirectoryIfMissing True dst
  content <- getDirectoryContents src
  let xs = filter (`notElem` [".", ".."]) content
  forM_ xs $ \name -> do
    let srcPath = src </> name
    let dstPath = dst </> name
    isDirectory <- doesDirectoryExist srcPath
    if isDirectory
      then copyDir srcPath dstPath
      else copyFile srcPath dstPath

  where
    doesFileOrDirectoryExist x = orM [doesDirectoryExist x, doesFileExist x]
    orM xs = or <$> sequence xs
    whenM s r = s >>= flip when r
