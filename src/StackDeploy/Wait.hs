module StackDeploy.Wait (waitForAccept) where

import Control.Concurrent (threadDelay)
import Control.Exception.Base (AssertionFailed(AssertionFailed))
import Control.Lens ((&), (.~), view)
import Control.Monad.Catch (throwM)
import Control.Monad.Trans.AWS (AWSConstraint)
import Data.Conduit (ConduitT, (.|), await, runConduit, yield)
import Data.Conduit.Combinators (find, iterM, takeWhile, yieldMany)
import Data.Conduit.List (consume)
import Data.Foldable (elem, null, toList)
import Data.Function (on)
import Data.List (reverse)
import Data.Set (Set)
import Data.String (String)
import Network.AWS (MonadAWS)
import Network.AWS.CloudFormation.DescribeStackEvents
  ( describeStackEvents
  , dseStackName
  , dsersStackEvents
  )
import Network.AWS.CloudFormation.Types
  ( ResourceStatus(..)
  , StackEvent
  , seClientRequestToken
  , seEventId
  , seResourceStatus
  , seResourceType
  )
import StackDeploy.AWS
import StackDeploy.Prelude
import StackDeploy.Types

-- | Wait for remote operation to end in a final status
--
-- Apply action for each event related to the remote operation.
waitForAccept
  :: forall m r . (AWSConstraint r m, MonadAWS m)
  => RemoteOperation
  -> (StackEvent -> m ())
  -> m RemoteOperationResult
waitForAccept remoteOperation action = classify =<< runConduit theEvents
  where
    classify :: Maybe StackEvent -> m RemoteOperationResult
    classify = maybe
      (throwM $ AssertionFailed "No last event")
      (maybe (throwM $ AssertionFailed "Last event has no resource status") remoteOperationResult . view seResourceStatus)

    remoteOperationResult :: ResourceStatus -> m RemoteOperationResult
    remoteOperationResult = \case
      CreateComplete         -> pure RemoteOperationSuccess
      CreateFailed           -> pure RemoteOperationFailure
      DeleteComplete         -> pure RemoteOperationSuccess
      DeleteFailed           -> pure RemoteOperationFailure
      RollbackComplete       -> pure RemoteOperationFailure
      RollbackFailed         -> pure RemoteOperationFailure
      UpdateComplete         -> pure RemoteOperationSuccess
      UpdateFailed           -> pure RemoteOperationFailure
      UpdateRollbackComplete -> pure RemoteOperationFailure
      status                 -> throwM . AssertionFailed $ message status
      where
        message :: Show a => a -> String
        message = ("Last event has unexpected resource status: " <>) . show

    theEvents :: ConduitT () b m (Maybe StackEvent)
    theEvents
      =  allEvents remoteOperation
      .| iterM action
      .| find (isStackEvent finalResourceStatus)

-- | Conduit polling for new stack events related to a remote operation
allEvents
  :: forall m r . (AWSConstraint r m, MonadAWS m)
  => RemoteOperation
  -> ConduitT () StackEvent m ()
allEvents remoteOperation@RemoteOperation{..} =
  go =<< getNext =<< getInitial
  where
    getNext :: [StackEvent] -> ConduitT () StackEvent m StackEvent
    getNext
      = maybe
          (throwM $ AssertionFailed "No start event")
          pure
      . listToMaybe

    initialEvents :: ConduitT () StackEvent m ()
    initialEvents
      =  stackEvents remoteOperation
      .| takeWhile (isToken token)
      .| takeUntilInclusive (isStackEvent initialResourceStatus)

    nextEvents :: StackEvent -> ConduitT () StackEvent m ()
    nextEvents lastEvent
      = stackEvents remoteOperation
      .| takeWhile (not . isExpectedEvent lastEvent)

    getInitial = do
      events <- poll initialEvents
      if null events
        then delay >> getInitial
        else pure events

    go :: StackEvent -> ConduitT () StackEvent m ()
    go lastEvent = do
      delay
      events <- poll (nextEvents lastEvent)
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

delay :: MonadIO m => m ()
delay = liftIO $ threadDelay 1_000_000  -- 1 second

stackEvents
  :: forall m r . (AWSConstraint r m, MonadAWS m)
  => RemoteOperation
  -> ConduitT () StackEvent m ()
stackEvents RemoteOperation{..} = listResource req dsersStackEvents
  where
    req = describeStackEvents & dseStackName .~ pure (toText stackId)


isStackEvent :: Set ResourceStatus -> StackEvent -> Bool
isStackEvent allowedStatus event = isStackEventType && isExpectedResourceStatus
  where
    resourceStatus = view seResourceStatus event

    isExpectedResourceStatus =
      resourceStatus `elem` (pure <$> toList allowedStatus)

    isStackEventType =
      view seResourceType event == pure "AWS::CloudFormation::Stack"

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

finalResourceStatus :: Set ResourceStatus
finalResourceStatus =
  [ CreateComplete
  , CreateFailed
  , DeleteComplete
  , DeleteFailed
  , RollbackComplete
  , UpdateComplete
  , UpdateFailed
  , UpdateRollbackComplete
  ]

initialResourceStatus :: Set ResourceStatus
initialResourceStatus =
  [ CreateInProgress
  , DeleteInProgress
  , UpdateInProgress
  ]

isToken :: Token -> StackEvent -> Bool
isToken token event = pure (toText token) == view seClientRequestToken event
