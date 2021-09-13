module AWS.Lambda.Runtime
  ( run
  )
where

import AWS.Lambda.Runtime.Prelude
import AWS.Lambda.Runtime.Types
import Control.Monad.Except (runExceptT)

import qualified AWS.Lambda.Runtime.Client as Client
import qualified Data.Aeson                as JSON

run
  :: forall m . (MonadCatch m, MonadIO m)
  => (LambdaEvent -> m JSON.Value)
  -> m ()
run function = do
  connection <- either throwM pure =<< liftIO (runExceptT Client.getConnection)

  forever $ processEvent connection function

processEvent
  :: forall m . (MonadIO m, MonadCatch m)
  => Client.Connection
  -> (LambdaEvent -> m JSON.Value)
  -> m ()
processEvent connection function = do
  event@Client.LambdaEvent{..} <- either throwM pure =<<
    liftIO (runExceptT $ Client.getNextLambdaEvent connection)

  Client.sendEventResponse connection requestId =<< function event
