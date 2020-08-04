module StackDeploy.Events (Poll(..), defaultPoll, pollEvents) where

import Control.Concurrent (threadDelay)
import Control.Exception.Base (AssertionFailed(AssertionFailed))
import Control.Lens ((.~), view)
import Data.Conduit (ConduitT, (.|), await, runConduit, yield)
import Data.Conduit.Combinators (find, iterM, takeWhile, yieldMany)
import Data.Conduit.List (consume)
import Data.Function (on)
import StackDeploy.AWS
import StackDeploy.Prelude
import StackDeploy.Types

import qualified Data.Foldable                                  as Foldable
import qualified Data.List                                      as List
import qualified Network.AWS.CloudFormation.DescribeStackEvents as CF
import qualified Network.AWS.CloudFormation.Types               as CF

data Poll = Poll
  { delay          :: forall m . MonadIO m => m ()
  , eventFilter    :: CF.StackEvent -> Bool
  , stackId        :: Id
  , startCondition :: CF.StackEvent -> Bool
  , stopCondition  :: CF.StackEvent -> Bool
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
  :: forall m . MonadAWS m
  => Poll
  -> (CF.StackEvent -> m ())
  -> m (Maybe CF.StackEvent)
pollEvents poll@Poll{..} eventAction = runConduit events
  where
    events :: ConduitT () b m (Maybe CF.StackEvent)
    events = allEvents poll .| iterM eventAction .| find stopCondition

-- | Conduit polling for new stack events
allEvents
  :: forall m . MonadAWS m
  => Poll
  -> ConduitT () CF.StackEvent m ()
allEvents Poll{..} =
  go =<< getNext =<< getInitial
  where
    getNext :: [CF.StackEvent] -> ConduitT () CF.StackEvent m CF.StackEvent
    getNext
      = maybe (throwM $ AssertionFailed "No start event") pure . listToMaybe

    initialEvents :: ConduitT () CF.StackEvent m ()
    initialEvents
      =  stackEvents stackId
      .| takeWhile eventFilter
      .| takeUntilInclusive startCondition

    nextEvents :: CF.StackEvent -> ConduitT () CF.StackEvent m ()
    nextEvents lastEvent
      = stackEvents stackId .| takeWhile (not . isExpectedEvent lastEvent)

    getInitial = do
      events <- poll initialEvents
      if Foldable.null events
        then delay >> getInitial
        else pure events

    go :: CF.StackEvent -> ConduitT () CF.StackEvent m ()
    go lastEvent = do
      delay
      events <- poll $ nextEvents lastEvent
      if Foldable.null events
        then go lastEvent
        else maybe (pure ()) go (listToMaybe events)

    isExpectedEvent :: CF.StackEvent -> CF.StackEvent -> Bool
    isExpectedEvent = (==) `on` view CF.seEventId

    poll
      :: ConduitT () CF.StackEvent m ()
      -> ConduitT () CF.StackEvent m [CF.StackEvent]
    poll action = do
      events <- action .| consume
      yieldMany $ List.reverse events
      pure events

stackEvents
  :: forall m . MonadAWS m
  => Id
  -> ConduitT () CF.StackEvent m ()
stackEvents stackId = listResource req CF.dsersStackEvents
  where
    req = CF.describeStackEvents & CF.dseStackName .~ pure (toText stackId)

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
