{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Exception (throw)
import           Control.Monad (when, forM_, unless)
import           Data.Aeson (encode, decode)
import           Data.Aeson.TH
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as Bz
import           Data.Char (toLower)
import           Data.Functor ((<$>))
import           Data.Map (Map, toList)
import           Data.Maybe (maybe)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Version (showVersion)
import           Options.Applicative
import           System.Directory (doesFileExist, doesDirectoryExist, removeDirectoryRecursive)
import           System.FilePath ((</>), (<.>), normalise)

import           Paths_libget (version)

import           CopyDir (copyDir)
import           Utils (jsonField)


data CmdOptions = CmdOptions
  { _file     :: Maybe String,
    _root     :: String,
    _packages :: String
  }

data Dependency = Dependency
  { _depName        :: !String
  , _depVersion     :: !String
  } deriving (Show, Eq)
$(deriveJSON defaultOptions {fieldLabelModifier=jsonField} ''Dependency)

data Spec = Spec
  { _specVersion      :: !Text
  , _specDependencies :: Map FilePath Dependency
  } deriving (Show, Eq)
$(deriveJSON defaultOptions {fieldLabelModifier=jsonField} ''Spec)


supportedSpecs = ["1"] :: [Text]

srcDir :: FilePath -> Dependency -> FilePath
srcDir packageDir (Dependency name ver) = packageDir </> name </> ver


copyDir' :: FilePath -> FilePath -> IO ()
copyDir' src dst = do
    putStrLn $ "Copying from " ++ src ++ " to " ++ dst
    copyDir src dst

crumbOf :: FilePath -> FilePath
crumbOf dst = dst </> ".libget"

leaveCrumb :: FilePath -> Dependency -> IO ()
leaveCrumb dst dep = Bz.writeFile (crumbOf dst) (encode dep)

checkCrumb :: FilePath -> Dependency -> IO Bool
checkCrumb dst dep = do
  exists <- doesFileExist crumb
  if exists
    then (maybe False (dep ==) . decode) <$> Bz.readFile crumb
    else return False
  where crumb = crumbOf dst

rmdir :: FilePath -> IO ()
rmdir dir = do
  exists <- doesDirectoryExist dir
  when exists (removeDirectoryRecursive dir)

main' :: CmdOptions -> IO ()
main' (CmdOptions file root packageDir) = do
  spec' <- decode <$> content
  case spec' of
    Nothing   -> throw $ userError "input could not be parsed as JSON"
    Just spec -> do
      when (_specVersion spec `notElem` supportedSpecs) $
        throw $ userError "input version is not supported"
      forM_ (toList $ _specDependencies spec) $ \(dst', dep) -> do
        let dst = normalise dst'
        upToDate <- checkCrumb dst dep
        unless upToDate $ do
          rmdir dst
          copyDir' (srcDir packageDir dep) dst
          leaveCrumb dst dep
  where
    content = maybe Bz.getContents Bz.readFile file


main :: IO ()
main = execParser opts >>= main'
  where
    opts = info (helper <*> addVersion <*> cmdOptions)
      ( fullDesc
     <> progDesc "Install dependencies for a project in a sandbox" )


addVersion :: Parser (a -> a)
addVersion = infoOption ("libget version " ++ showVersion version)
  ( long "version"
  <> help "Show version information" )


cmdOptions :: Parser CmdOptions
cmdOptions = CmdOptions
  <$> (optional . strOption)
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
  <*> strOption
      ( long    "packages"
     <> short   'p'
     <> metavar "DIR"
     <> help    "Root path to packages" )
