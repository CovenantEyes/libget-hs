{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Utils
  ( jsonField
  , SafeTerm
  , stdTerm
  , runSafeTerm
  , runSafeTermWith
  , sprint
  , sprintError
  ) where

import Control.Applicative (Applicative)
import Control.Concurrent (MVar, withMVar, newMVar)
import Data.Char
import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Control.Monad.Trans (MonadIO, liftIO)
import System.IO (stdout, stderr, hPutStrLn, Handle)


data Terminal = Terminal { _std :: Handle, _err :: Handle }

printTerm, printTermErr :: Terminal -> String -> IO ()
printTerm    (Terminal std _)   = hPutStrLn std
printTermErr (Terminal _   err) = hPutStrLn err


newtype SafeTerm a = SafeTerm
  { doSafeTerm :: ReaderT (MVar Terminal) IO a
  } deriving (Applicative, Monad, MonadIO, MonadReader (MVar Terminal), Functor)


stdTerm :: Terminal
stdTerm = Terminal stdout stderr


runSafeTerm :: SafeTerm a -> IO a
runSafeTerm a = do
  term <- newMVar stdTerm
  runSafeTermWith term a


runSafeTermWith :: MVar Terminal -> SafeTerm a -> IO a
runSafeTermWith term a = runReaderT (doSafeTerm a) term


sprint, sprintError :: String -> SafeTerm ()
sprint msg = do
  term <- ask
  liftIO $ withMVar term (`printTerm` msg)

sprintError msg = do
  term <- ask
  liftIO $ withMVar term (`printTermErr` msg)


jsonField :: String -> String
jsonField = map toLower . dropWhile (not . isUpper)

