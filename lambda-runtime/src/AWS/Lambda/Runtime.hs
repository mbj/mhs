module AWS.Lambda.Runtime
  ( module AWS.Lambda.Runtime.Types
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
  => (Event -> m JSON.Value)
  -> m ()
run function = do
  connection <- either throwM pure =<< liftIO (runExceptT Client.getConnection)

  forever $ processEvent connection function

processEvent
  :: forall m . (MonadIO m, MonadCatch m)
  => Client.Connection
  -> (Event -> m JSON.Value)
  -> m ()
processEvent connection function = do
  event@Event{..} <- either throwM pure =<<
    liftIO (runExceptT $ Client.getNextEvent connection)

  Client.sendEventResponse connection requestId =<< function event
