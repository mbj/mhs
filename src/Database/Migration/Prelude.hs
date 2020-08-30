module Database.Migration.Prelude
  ( module Exports
  , eitherFail
  , log
  )
where

import Control.Monad.IO.Unlift as Exports (MonadUnliftIO)
import Data.Conversion         as Exports (Conversion(..), convertUnsafe)
import Data.Functor            as Exports (fmap)
import MPrelude                as Exports

import qualified Data.Text.IO as Text
import qualified System.IO    as IO

eitherFail :: (MonadFail m, Show e) => Either e a -> m a
eitherFail = either (fail . show) pure

log :: (ToText a, MonadIO m) => a -> m ()
log = liftIO . Text.hPutStrLn IO.stderr . toText
