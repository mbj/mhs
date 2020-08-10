module Database.Migration.Prelude
  ( module Exports
  , eitherFail
  , log
  , show
  )
where

import Control.Monad.IO.Unlift as Exports (MonadUnliftIO)
import Data.Conversion         as Exports (Conversion(..), convertUnsafe)
import Data.Functor            as Exports (fmap)
import MPrelude                as Exports hiding (show)

import qualified Data.String  as String
import qualified Data.Text.IO as Text
import qualified GHC.Show     as Show
import qualified System.IO    as IO

eitherFail :: (MonadFail m, Show e) => Either e a -> m a
eitherFail = either (fail . show) pure

log :: (ToText a, MonadIO m) => a -> m ()
log = liftIO . Text.hPutStrLn IO.stderr . toText

show :: (Show a, String.IsString b) => a -> b
show = String.fromString . Show.show
