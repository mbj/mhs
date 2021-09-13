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
run = forever . processEvent

processEvent
  :: forall m . (MonadIO m, MonadCatch m)
  => (JSON.Value -> m JSON.Value)
  -> m ()
processEvent fn =
  either throwM runFunction =<< liftIO (runExceptT Client.getHttpConfig)
  where
    runFunction :: Client.HTTPConfig -> m ()
    runFunction httpConfig = do
      Client.LambdaEvent{..} <- processNextLambdaAction httpConfig
        $ Client.getNextLambdaEvent httpConfig

      Client.sendEventResponse httpConfig requestId =<< fn event

    processNextLambdaAction :: Client.HTTPConfig -> Client.LambdaClient a -> m a
    processNextLambdaAction httpConfig action =
      liftIO (runExceptT action) >>= \case
        Right result -> pure result
        Left error   -> do
          Client.sendBootError httpConfig error
          -- throw error to give a chance to a client to catch and respond to error
          throwM error
