{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Monad.Script
  ( Script
  , runScript
  , runIO
  ) where

import           Prelude

import           Control.Exception.Safe


newtype Script a = Script (IO a)
  deriving newtype (Functor, Applicative, Monad, MonadThrow, MonadCatch)


runScript :: Script a -> IO (Either SomeException a)
runScript (Script io) = tryAny io


runIO :: (IOException -> IO a) -> IO a -> Script a
runIO handler io = Script (handleIO handler io)
