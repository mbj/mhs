module AWS.Lambda.Runtime
  ( run
  )
where

import AWS.Lambda.Runtime.Prelude
import Control.Monad.Except (runExceptT)

import qualified AWS.Lambda.Runtime.Client as Client
import qualified Data.Aeson                as JSON

run
  :: forall m . (MonadCatch m, MonadIO m)
  => (JSON.Value -> m JSON.Value)
  -> m ()
run function = do
  connection <- either throwM pure =<< liftIO (runExceptT Client.getConnection)

  forever $ processEvent connection function

processEvent
  :: forall m . (MonadIO m, MonadCatch m)
  => Client.Connection
  -> (JSON.Value -> m JSON.Value)
  -> m ()
processEvent connection function = do
  Client.LambdaEvent{..} <- processNextLambdaAction
    $ Client.getNextLambdaEvent connection

  Client.sendEventResponse connection requestId =<< function event
  where
    processNextLambdaAction :: Client.LambdaClient a -> m a
    processNextLambdaAction action =
      liftIO (runExceptT action) >>= \case
        Right result -> pure result
        Left error   -> do
          Client.sendBootError connection error
          -- throw error to give a chance to a client to catch and respond to error
          throwM error
