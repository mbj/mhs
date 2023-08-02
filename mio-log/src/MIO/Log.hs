module MIO.Log
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
import MIO.Core
import MIO.Log.Formatter
import MIO.Log.Types
import MPrelude

import qualified Data.ByteString.Char8    as BS
import qualified Data.Text.Encoding       as Text
import qualified Data.Time.Clock          as Clock
import qualified System.IO                as IO

newtype Action = Action (Message -> IO ())

type Env env = HasField "logAction" env Action

log :: Env env => Severity -> Text -> MIO env ()
log severity message = do
  time     <- liftIO Clock.getCurrentTime
  threadId <- liftIO myThreadId
  logMessage Message{..}

debug :: Env env => Text -> MIO env ()
debug = log Debug

info :: Env env => Text -> MIO env ()
info = log Info

warn :: Env env => Text -> MIO env ()
warn = log Warn

error :: Env env => Text -> MIO env ()
error = log Error

exception :: forall e env . (Env env, Exception e) => e -> MIO env ()
exception = error . convert . displayException

logMessage :: forall env . Env env => Message -> MIO env ()
logMessage message = asks (.logAction) >>= (\(Action action) -> liftIO $ action message)

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
