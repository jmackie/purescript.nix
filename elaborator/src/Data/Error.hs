{-# LANGUAGE ExistentialQuantification #-}
module Data.Error
  ( Error(..)
  , raise
  , raiseString
  , raiseWithContext
  , describe
  ) where

import qualified Data.List              as List
import qualified Data.Text              as Text
import           Prelude

import           Control.Exception.Safe hiding (StringException)
import           Data.Text              (Text)

data Error = forall e. Exception e => Error e [Context]

instance Exception Error
instance Show Error where
  show (Error root breadcrumbs) =
    List.intercalate ": " . ("Error":) . reverse $
      show root : fmap (Text.unpack . unContext) breadcrumbs


newtype Context = Context { unContext :: Text }


newtype StringException = StringException String

instance Exception StringException
instance Show StringException where
  show (StringException s) = s


raise :: (MonadThrow m, Exception e) => e -> m a
raise e = throw (Error e [])


raiseString :: (MonadThrow m) => String -> m a
raiseString = raise . StringException


raiseWithContext :: (MonadThrow m, Exception e) => Text -> e -> m a
raiseWithContext ctx e = throw (Error e [Context ctx])


describe :: (MonadCatch m) => Text -> m a -> m a
describe ctx = flip catch $
  \(Error root crumbs) -> throw (Error root (crumbs <> [Context ctx]))
