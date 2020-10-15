module Database.Migration.Prelude
  ( module Exports
  , eitherThrowIO
  )
where

import Data.Conversions as Exports
import MPrelude         as Exports

import qualified UnliftIO.Exception as Exception

eitherThrowIO :: (Exception.Exception e, MonadIO m) => Either e a -> m a
eitherThrowIO = either Exception.throwIO pure
