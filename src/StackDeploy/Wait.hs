module StackDeploy.Wait (waitForAccept) where

import Control.Exception.Base (AssertionFailed(AssertionFailed))
import Control.Lens (view)
import Control.Monad.Catch (throwM)
import Data.Foldable (elem, toList)
import Data.Set (Set)
import Data.String (String)
import Network.AWS.CloudFormation.Types
import StackDeploy.AWS
import StackDeploy.Events
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
waitForAccept RemoteOperation{..} action =
  classify =<< pollEvents pollConfig action
  where
    classify :: Maybe StackEvent -> m RemoteOperationResult
    classify = maybe
      (throwM $ AssertionFailed "No last event")
      assertPresentResourceStatus

    assertPresentResourceStatus =
      maybe
        (throwM $ AssertionFailed "Last event has no resource status")
        remoteOperationResult
        . view seResourceStatus

    pollConfig = (defaultPoll stackId)
      { eventFilter    = isToken token
      , startCondition = isStackEvent initialResourceStatus
      , stopCondition  = isStackEvent finalResourceStatus
      }

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

isStackEvent :: Set ResourceStatus -> StackEvent -> Bool
isStackEvent allowedStatus event = isStackEventType && isExpectedResourceStatus
  where
    resourceStatus = view seResourceStatus event

    isExpectedResourceStatus =
      resourceStatus `elem` (pure <$> toList allowedStatus)

    isStackEventType =
      view seResourceType event == pure "AWS::CloudFormation::Stack"

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
