module AWS.Lambda.Runtime
  ( module AWS.Lambda.Runtime.Types
  , Client.getConnection
  , processNextEvent
  , run
  , runTraced
  )
where

import AWS.Lambda.Runtime.Prelude
import AWS.Lambda.Runtime.Types
import Control.Monad.Except (runExceptT)
import Data.Conversions.FromType
import MRIO.Core

import qualified AWS.Lambda.Runtime.Client as Client
import qualified Data.Aeson                as JSON
import qualified XRay
import qualified XRay.Segment              as XRay
import qualified XRay.TraceHeader          as XRay

run
  :: forall m . (MonadCatch m, MonadIO m)
  => (Event JSON.Value -> m JSON.Value)
  -> m ()
run function = do
  connection <- eitherThrow =<< liftIO (runExceptT Client.getConnection)

  forever $ processNextEvent connection function

runTraced
  :: (XRay.Env env, JSON.FromJSON a, JSON.ToJSON b)
  => (Event a -> XRay.Segment -> XRay.Segment)
  -> (b -> XRay.Segment -> XRay.Segment)
  -> (TracedEvent a -> RIO env b)
  -> RIO env ()
runTraced initSegment finishSegment action = do
  connection <- eitherThrow =<< liftIO (runExceptT Client.getConnection)

  forever $ do
    event@Event{traceHeader = traceHeader@XRay.TraceHeader{..}, ..} <-
      eitherThrow =<< liftIO (runExceptT $ Client.getNextEvent connection)

    result <- XRay.withSegment
      (fromType @"Event")
      traceId
      parentId
      (initSegment event)
      finishSegment
      (\segmentId -> action TracedEvent{..})

    Client.sendEventResponse connection requestId (JSON.toJSON result)

processNextEvent
  :: forall m . (MonadIO m, MonadCatch m)
  => Client.Connection
  -> (Event JSON.Value -> m JSON.Value)
  -> m ()
processNextEvent connection function = do
  event@Event{..} <- eitherThrow =<< liftIO (runExceptT $ Client.getNextEvent connection)

  Client.sendEventResponse connection requestId =<< function event
