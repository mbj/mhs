module DBT.Prelude (module MPrelude, log) where

import MPrelude

import qualified Data.Text.IO as Text
import qualified System.IO    as IO

log :: forall a m . (MonadIO m, ToText a) => a -> m ()
log = liftIO . Text.hPutStrLn IO.stderr . convertText
