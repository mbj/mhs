module CBT.Prelude (module Exports, module MPrelude, debug, log) where

import Control.Monad.IO.Unlift as Exports (MonadUnliftIO)
import MPrelude

import qualified Data.Text.IO       as Text
import qualified System.Environment as Environment
import qualified System.IO          as IO

log :: forall a m . (MonadIO m, ToText a) => a -> m ()
log = liftIO . Text.hPutStrLn IO.stderr . convertText

debug :: forall a m . (MonadIO m, ToText a) => a -> m ()
debug message =
  liftIO (Environment.lookupEnv "CBT_DEBUG")
    >>= maybe (pure ()) (const (log message))
