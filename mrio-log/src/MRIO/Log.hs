{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}

module MRIO.Log
  ( Action(..)
  , Env
  , Message(..)
  , Severity(..)
  , debug
  , defaultCLIAction
  , error
  , exception
  , formatCLIAction
  , info
  , noopAction
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

newtype Action = Action (Message -> IO ())

type Env env = HasField "logAction" env Action

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
logMessage message =
  asks (getField @"logAction")
    >>= (\(Action action) -> liftIO $ action message)

formatCLIAction :: Formatter -> Action
formatCLIAction formatter
  = Action
  $ BS.hPutStrLn IO.stderr
  . Text.encodeUtf8
  . render formatter

defaultCLIAction :: Action
defaultCLIAction = formatCLIAction defaultCLIFormatter

noopAction :: Action
noopAction = Action . const $ pure ()
