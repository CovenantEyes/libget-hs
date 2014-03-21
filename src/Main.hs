{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Exception (throw)
import           Control.Monad (when, unless)
import           Control.Monad.Trans (liftIO)
import           Control.Concurrent.Async (mapConcurrently)
import           Control.Conditional (ifM, whenM)
import           Control.Monad.Loops (firstM)
import           Data.Aeson (encode, decode)
import           Data.Aeson.TH
import qualified Data.ByteString.Lazy as Bz
import           Data.Map.Strict (Map, toList)
import           Data.Version (showVersion)
import           Options.Applicative
import           System.Directory (doesFileExist, doesDirectoryExist, removeDirectoryRecursive)
import           System.Exit (exitFailure)
import           System.FilePath ((</>), normalise)

import           Paths_libget (version)

import           CopyDir (copyDir)
import           Utils (jsonField, SafeTerm, runSafeTerm, sprintError, sprint)


data CmdOptions = CmdOptions
  { _optPackageRoots :: [FilePath]
  , _optFile         :: Maybe FilePath
  , _optRoot         :: FilePath
  }

-- IMPROVE: Parse spec as JSON AST first so we can change parsing algorithms based on
--          the spec version number. As it stands, this will fail to parse anything
--          that does not exactly match this structure.
data Dependency = Dependency
  { _depName        :: !String
  , _depVersion     :: !String
  } deriving (Eq)
$(deriveJSON defaultOptions {fieldLabelModifier=jsonField} ''Dependency)

instance Show Dependency where
  show (Dependency name ver) =  name ++ "@" ++ ver

data Spec = Spec
  { _specVersion      :: !String
  , _specDependencies :: Map FilePath Dependency
  } deriving (Show, Eq)
$(deriveJSON defaultOptions {fieldLabelModifier=jsonField} ''Spec)


supportedSpecs :: [String]
supportedSpecs = ["1"]


copyDir' :: FilePath -> FilePath -> SafeTerm ()
copyDir' src dst = do
    sprint $ "Copying from " ++ src ++ " to " ++ dst
    liftIO $ copyDir src dst


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


removeDirSafely :: FilePath -> IO ()
removeDirSafely dir =
  whenM (doesDirectoryExist dir) $ removeDirectoryRecursive dir


install :: [FilePath] -> FilePath -> Dependency -> SafeTerm Bool
install packageRoots dst dep = do
  done <- liftIO $ alreadyUpToDate dst dep
  if done
    then return True
    else do
      src' <- liftIO $ firstM doesDirectoryExist (packageDir dep <$> packageRoots)
      case src' of
        Nothing -> do
          sprintError $ "failed to find package for dependency " ++ show dep
          return False
        Just src -> do
          installFrom src
          return True
  where
    packageDir (Dependency name ver) packageRoot = packageRoot </> name </> ver

    installFrom src = do
      liftIO $ removeDirSafely dst
      copyDir' src dst
      liftIO $ leaveCrumb dst dep


main' :: CmdOptions -> IO ()
main' (CmdOptions packageRoots file root) = runSafeTerm $ do
  spec' <- liftIO $ decode <$> liftIO content
  case spec' of
    Nothing   -> throw $ userError "did not understand specification input"
    Just spec -> handleSpec spec
  where
    content = maybe Bz.getContents Bz.readFile file

    handleSpec spec = do
      when (_specVersion spec `notElem` supportedSpecs) $
        throw $ userError "input version is not supported"

      results <- mapM installAtRoot (toList $ _specDependencies spec)
      liftIO $ unless (and results) exitFailure

    installAtRoot :: (FilePath, Dependency) -> SafeTerm Bool
    installAtRoot (dst, dep) = install packageRoots (root </> normalise dst) dep


progName :: String
progName = "libget"


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
