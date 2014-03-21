{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Utils
  ( jsonField
  , SafeTerm
  , stdTerm
  , runSafeTermWith
  , sprint
  , sprintError
  , io
  ) where

import Control.Applicative (Applicative)
import Control.Concurrent (MVar, withMVar)
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


runSafeTermWith :: MVar Terminal -> SafeTerm a -> IO a
runSafeTermWith term a = runReaderT (doSafeTerm a) term


sprint, sprintError :: String -> SafeTerm ()
sprint      msg = ask >>= io . (`withMVar` (`printTerm` msg))
sprintError msg = ask >>= io . (`withMVar` (`printTermErr` msg))


io :: MonadIO m => IO a -> m a
io = liftIO


-- Helper function to "undecorate" record field names
-- I.e. jsonField "_recordName" == "name"
jsonField :: String -> String
jsonField = map toLower . dropWhile (not . isUpper)

