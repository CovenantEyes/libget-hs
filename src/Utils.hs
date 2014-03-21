{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Utils
  ( jsonField
  , SafeTerm
  , runSafeTerm
  , sprint
  , sprintError
  ) where

import Control.Applicative (Applicative, (<$>))
import Control.Concurrent (MVar, withMVar)
import Data.Char
import Control.Monad.Reader (MonadReader, ReaderT, ask)
import Control.Monad.Trans (MonadIO, liftIO)
import System.IO (stdout, stderr, hPutStrLn, Handle)


data Terminal = Terminal { _std :: Handle, _err :: Handle }

printTerm, printTermErr :: Terminal -> String -> IO ()
printTerm    (Terminal std _)   = hPutStrLn std
printTermErr (Terminal _   err) = hPutStrLn err


newtype SafeTerm a = SafeTerm
  { runSafeTerm :: ReaderT (MVar Terminal) IO a
  } deriving (Applicative, Monad, MonadIO, MonadReader (MVar Terminal), Functor)

sprint, sprintError :: String -> SafeTerm ()
sprint msg = do
  term <- ask
  liftIO $ withMVar term (`printTerm` msg)

sprintError msg = do
  term <- ask
  liftIO $ withMVar term (`printTermErr` msg)


jsonField :: String -> String
jsonField = map toLower . dropWhile (not . isUpper)

