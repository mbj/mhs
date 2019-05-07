module StackDeploy.Events (Poll(..), defaultPoll, pollEvents) where

import Control.Concurrent (threadDelay)
import Control.Exception.Base (AssertionFailed(AssertionFailed))
import Control.Lens ((&), (.~), view)
import Control.Monad.Catch (throwM)
import Control.Monad.Trans.AWS (AWSConstraint)
import Data.Conduit (ConduitT, (.|), await, runConduit, yield)
import Data.Conduit.Combinators (find, iterM, takeWhile, yieldMany)
import Data.Conduit.List (consume)
import Data.Foldable (null)
import Data.Function (on)
import Data.List (reverse)
import Network.AWS (MonadAWS)
import Network.AWS.CloudFormation.DescribeStackEvents
  ( describeStackEvents
  , dseStackName
  , dsersStackEvents
  )
import Network.AWS.CloudFormation.Types (StackEvent, seEventId)
import StackDeploy.AWS
import StackDeploy.Prelude
import StackDeploy.Types

data Poll = Poll
  { delay          :: forall m . MonadIO m => m ()
  , eventFilter    :: StackEvent -> Bool
  , stackId        :: Id
  , startCondition :: StackEvent -> Bool
  , stopCondition  :: StackEvent -> Bool
  }

defaultPoll :: Id -> Poll
defaultPoll stackId = Poll
  { delay          = liftIO $ threadDelay 1_000_000  -- 1 second
  , eventFilter    = const True                      -- accept all events
  , startCondition = const True                      -- start with any event
  , stopCondition  = const False                     -- never stop
  , ..
  }

-- | Wait for remote operation to end in a final status
--
-- Apply action for each event related to the remote operation.
pollEvents
  :: forall m r . (AWSConstraint r m, MonadAWS m)
  => Poll
  -> (StackEvent -> m ())
  -> m (Maybe StackEvent)
pollEvents poll@Poll{..} eventAction = runConduit events
  where
    events :: ConduitT () b m (Maybe StackEvent)
    events = allEvents poll .| iterM eventAction .| find stopCondition

-- | Conduit polling for new stack events
allEvents
  :: forall m r . (AWSConstraint r m, MonadAWS m)
  => Poll
  -> ConduitT () StackEvent m ()
allEvents Poll{..} =
  go =<< getNext =<< getInitial
  where
    getNext :: [StackEvent] -> ConduitT () StackEvent m StackEvent
    getNext
      = maybe (throwM $ AssertionFailed "No start event") pure . listToMaybe

    initialEvents :: ConduitT () StackEvent m ()
    initialEvents
      =  stackEvents stackId
      .| takeWhile eventFilter
      .| takeUntilInclusive startCondition

    nextEvents :: StackEvent -> ConduitT () StackEvent m ()
    nextEvents lastEvent
      = stackEvents stackId .| takeWhile (not . isExpectedEvent lastEvent)

    getInitial = do
      events <- poll initialEvents
      if null events
        then delay >> getInitial
        else pure events

    go :: StackEvent -> ConduitT () StackEvent m ()
    go lastEvent = do
      delay
      events <- poll $ nextEvents lastEvent
      if null events
        then go lastEvent
        else maybe (pure ()) go (listToMaybe events)

    isExpectedEvent :: StackEvent -> StackEvent -> Bool
    isExpectedEvent = (==) `on` view seEventId

    poll
      :: ConduitT () StackEvent m ()
      -> ConduitT () StackEvent m [StackEvent]
    poll action = do
      events <- action .| consume
      yieldMany $ reverse events
      pure events

stackEvents
  :: forall m r . (AWSConstraint r m, MonadAWS m)
  => Id
  -> ConduitT () StackEvent m ()
stackEvents stackId = listResource req dsersStackEvents
  where
    req = describeStackEvents & dseStackName .~ pure (toText stackId)

-- | takeUntil but returns the first matching item instead of discarding it
takeUntilInclusive :: forall a m . Monad m => (a -> Bool) -> ConduitT a a m ()
takeUntilInclusive predicate = go
  where
    go = evaluate =<< await

    evaluate :: Maybe a -> ConduitT a a m ()
    evaluate = maybe (pure ()) applyPredicate

    applyPredicate item = do
      yield item
      if predicate item
        then pure ()
        else go
