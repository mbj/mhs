{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}

module MRIO.Log
  ( Action
  , Env(..)
  , Message(..)
  , Severity(..)
  , debug
  , defaultCLIAction
  , error
  , exception
  , formatCLIAction
  , info
  , warn
  )
where

import Control.Concurrent (myThreadId)
import Control.Monad.Reader (asks)
import Data.Conversions
import Data.Function
import GHC.Exception (Exception, displayException)
import GHC.Records (HasField, getField)
import MPrelude
import MRIO.Core
import MRIO.Log.Formatter
import MRIO.Log.Types

import qualified Data.ByteString.Char8    as BS
import qualified Data.Text.Encoding       as Text
import qualified Data.Time.Clock          as Clock
import qualified System.IO                as IO

type Action m = Message -> m ()

class Env env where
  getLogAction :: env -> Action (RIO env)

instance (HasField "logAction" env (Action (RIO env))) => Env env where
  getLogAction env = getField @"logAction" env

log :: Env env => Severity -> Text -> RIO env ()
log severity message = do
  time     <- liftIO Clock.getCurrentTime
  threadId <- liftIO myThreadId
  logMessage Message{..}

debug :: Env env => Text -> RIO env ()
debug = log Debug

info :: Env env => Text -> RIO env ()
info = log Info

warn :: Env env => Text -> RIO env ()
warn = log Warn

error :: Env env => Text -> RIO env ()
error = log Error

exception :: forall e env . (Env env, Exception e) => e -> RIO env ()
exception = error . convert . displayException

logMessage :: forall env . Env env => Message -> RIO env ()
logMessage message = ($ message) =<< asks getLogAction

formatCLIAction :: MonadIO m => Formatter -> Action m
formatCLIAction formatter
  = (liftIO . BS.hPutStrLn IO.stderr)
  . Text.encodeUtf8
  . render formatter

defaultCLIAction :: MonadIO m => Action m
defaultCLIAction = formatCLIAction defaultCLIFormatter
