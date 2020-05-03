module LHT.Prelude (module Exports, log) where

import Control.Monad.IO.Unlift as Exports (MonadUnliftIO)
import MPrelude                as Exports

import qualified Data.Text.IO as Text
import qualified System.IO    as IO

log :: forall a m . (ToText a, MonadIO m) => a -> m ()
log = liftIO . Text.hPutStrLn IO.stderr . toText
