{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Exception (throw)
import           Control.Monad (when, unless, forM)
import           Control.Concurrent.Async (mapConcurrently)
import           Control.Conditional (unlessM, ifM)
import           Data.Aeson (encode, decode)
import           Data.Aeson.TH
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as Bz
import           Data.Char (toLower)
import           Data.Functor ((<$>))
import           Data.Map (Map, toList)
import           Data.Maybe (maybe, catMaybes)
import           Data.Version (showVersion)
import           Options.Applicative
import           System.Directory (doesFileExist, doesDirectoryExist, removeDirectoryRecursive)
import           System.Exit (exitFailure)
import           System.FilePath ((</>), (<.>), normalise)
import           System.IO (stderr, hPutStrLn)

import           Paths_libget (version)

import           CopyDir (copyDir)
import           Utils (jsonField)


data CmdOptions = CmdOptions
  { _optPackageRoots :: [FilePath]
  , _optFile         :: Maybe FilePath
  , _optRoot         :: FilePath
  }

data Dependency = Dependency
  { _depName        :: !String
  , _depVersion     :: !String
  } deriving (Eq)
$(deriveJSON defaultOptions {fieldLabelModifier=jsonField} ''Dependency)

instance Show Dependency where
  show (Dependency name ver) = "dependency " ++ name ++ "@" ++ ver

data Spec = Spec
  { _specVersion      :: !String
  , _specDependencies :: Map FilePath Dependency
  } deriving (Show, Eq)
$(deriveJSON defaultOptions {fieldLabelModifier=jsonField} ''Spec)


supportedSpecs = ["1"] :: [String]


copyDir' :: FilePath -> FilePath -> IO ()
copyDir' src dst = do
    putStrLn $ "Copying from " ++ src ++ " to " ++ dst
    copyDir src dst


crumbOf :: FilePath -> FilePath
crumbOf dst = dst </> ".libget"


leaveCrumb :: FilePath -> Dependency -> IO ()
leaveCrumb dst dep = Bz.writeFile (crumbOf dst) (encode dep)


alreadyUpToDate :: FilePath -> Dependency -> IO Bool
alreadyUpToDate dst dep =
  ifM (doesFileExist crumb)
    ((maybe False (dep ==) . decode) <$> Bz.readFile crumb)
    (return False)
  where crumb = crumbOf dst


rmdir :: FilePath -> IO ()
rmdir dir = doesDirectoryExist dir >>= (`when` removeDirectoryRecursive dir)


first :: (Monad m, Functor m) => (a -> m (Maybe b)) -> [a] -> m (Maybe b)
first f as = (safeHead . catMaybes) <$> mapM f as
  where safeHead [] = Nothing
        safeHead xs = Just (head xs)


errorMsg :: String -> IO ()
errorMsg msg = hPutStrLn stderr $ "Error: " ++ msg


install :: [FilePath] -> FilePath -> Dependency -> IO Bool
install packageRoots dst dep = do
  done <- alreadyUpToDate dst dep
  if done
    then return True
    else do
      src' <- first existing (packageDir dep <$> packageRoots)
      case src' of
        Nothing -> do
          errorMsg $ "failed to find package for " ++ show dep
          return False
        Just src -> do
          rmdir dst
          copyDir' src dst
          leaveCrumb dst dep
          return True
  where
    existing dir = ifM (doesDirectoryExist dir) (return $ Just dir) (return Nothing)
    packageDir (Dependency name ver) packageRoot = packageRoot </> name </> ver


main' :: CmdOptions -> IO ()
main' (CmdOptions packageRoots file root) = do
  spec' <- decode <$> content
  case spec' of
    Nothing   -> throw $ userError "did not understand specification input"
    Just spec -> handleSpec spec
  where
    content = maybe Bz.getContents Bz.readFile file

    handleSpec spec = do
      when (_specVersion spec `notElem` supportedSpecs) $
        throw $ userError "input version is not supported"

      results <- mapConcurrently installAtRoot (toList $ _specDependencies spec)
      unless (and results) exitFailure

    installAtRoot (dst, dep) = install packageRoots (root </> normalise dst) dep


progName = "libget" :: String

main :: IO ()
main = execParser opts >>= main'
  where
    opts = info (helper <*> addVersion <*> cmdOptions)
      ( fullDesc
     <> progDesc "Install dependencies for a project in a sandbox" )


addVersion :: Parser (a -> a)
addVersion = infoOption (progName ++ " version " ++ showVersion version)
  ( long "version"
  <> help "Show version information")


cmdOptions :: Parser CmdOptions
cmdOptions = CmdOptions
  <$> some (argument str (metavar "PACKAGE_ROOTS..."))
  <*> (optional . strOption)
      ( long    "file"
     <> short   'f'
     <> metavar "FILE"
     <> help    "Read FILE instead of stdin" )
  <*> strOption
      ( long    "root"
     <> short   'r'
     <> metavar "ROOT"
     <> value   "."
     <> showDefault
     <> help    "Path to root of project" )
