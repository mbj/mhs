module AWS.Lambda.Runtime
  ( module AWS.Lambda.Runtime.Types
  , Client.getConnection
  , processNextEvent
  , run
  )
where

import AWS.Lambda.Runtime.Prelude
import AWS.Lambda.Runtime.Types
import Control.Monad.Except (runExceptT)

import qualified AWS.Lambda.Runtime.Client as Client
import qualified Data.Aeson                as JSON

run
  :: forall m . (MonadCatch m, MonadIO m)
  => (Event JSON.Value -> m JSON.Value)
  -> m ()
run function = do
  connection <- eitherThrow =<< liftIO (runExceptT Client.getConnection)

  forever $ processNextEvent connection function

processNextEvent
  :: forall m . (MonadIO m, MonadCatch m)
  => Client.Connection
  -> (Event JSON.Value -> m JSON.Value)
  -> m ()
processNextEvent connection function = do
  event@Event{..} <- eitherThrow =<< liftIO (runExceptT $ Client.getNextEvent connection)

  Client.sendEventResponse connection requestId =<< function event
