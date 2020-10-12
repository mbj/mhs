module DBT.Prelude (module Exports, debug, log) where

import Data.Conversions as Exports
import GHC.Records      as Exports (getField)
import MPrelude         as Exports
import MRIO.Core        as Exports

import qualified Data.Text.IO       as Text
import qualified System.Environment as Environment
import qualified System.IO          as IO

log :: forall a m . (MonadIO m, ToText a) => a -> m ()
log = liftIO . Text.hPutStrLn IO.stderr . convertText

debug :: forall a m . (MonadIO m, ToText a) => a -> m ()
debug = onDebug . log

onDebug :: MonadIO m => m () -> m ()
onDebug action =
  liftIO (Environment.lookupEnv "DBT_DEBUG")
    >>= maybe (pure ()) (const action)
