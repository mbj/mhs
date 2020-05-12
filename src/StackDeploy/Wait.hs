module StackDeploy.Wait (waitForAccept) where

import Control.Exception.Base (AssertionFailed(AssertionFailed))
import Control.Lens (view)
import Data.Set (Set)
import StackDeploy.AWS
import StackDeploy.Events
import StackDeploy.Prelude
import StackDeploy.Types

import qualified Data.Foldable                    as Foldable
import qualified Network.AWS.CloudFormation.Types as CF

-- | Wait for remote operation to end in a final status
--
-- Apply action for each event related to the remote operation.
waitForAccept
  :: forall m r . (AWSConstraint r m, MonadAWS m)
  => RemoteOperation
  -> (CF.StackEvent -> m ())
  -> m RemoteOperationResult
waitForAccept RemoteOperation{..} action =
  classify =<< pollEvents pollConfig action
  where
    classify :: Maybe CF.StackEvent -> m RemoteOperationResult
    classify = maybe
      (throwM $ AssertionFailed "No last event")
      assertPresentResourceStatus

    assertPresentResourceStatus =
      maybe
        (throwM $ AssertionFailed "Last event has no resource status")
        remoteOperationResult
        . view CF.seResourceStatus

    pollConfig = (defaultPoll stackId)
      { eventFilter    = isToken token
      , startCondition = isStackEvent initialResourceStatus
      , stopCondition  = isStackEvent finalResourceStatus
      }

    remoteOperationResult :: CF.ResourceStatus -> m RemoteOperationResult
    remoteOperationResult = \case
      CF.CreateComplete         -> pure RemoteOperationSuccess
      CF.CreateFailed           -> pure RemoteOperationFailure
      CF.DeleteComplete         -> pure RemoteOperationSuccess
      CF.DeleteFailed           -> pure RemoteOperationFailure
      CF.RollbackComplete       -> pure RemoteOperationFailure
      CF.RollbackFailed         -> pure RemoteOperationFailure
      CF.UpdateComplete         -> pure RemoteOperationSuccess
      CF.UpdateFailed           -> pure RemoteOperationFailure
      CF.UpdateRollbackComplete -> pure RemoteOperationFailure
      status                    -> throwM . AssertionFailed $ message status
      where
        message :: Show a => a -> String
        message = ("Last event has unexpected resource status: " <>) . show

isStackEvent :: Set CF.ResourceStatus -> CF.StackEvent -> Bool
isStackEvent allowedStatus event = isStackEventType && isExpectedResourceStatus
  where
    resourceStatus = view CF.seResourceStatus event

    isExpectedResourceStatus =
      resourceStatus `Foldable.elem` (pure <$> Foldable.toList allowedStatus)

    isStackEventType =
      view CF.seResourceType event == pure "AWS::CloudFormation::Stack"

finalResourceStatus :: Set CF.ResourceStatus
finalResourceStatus =
  [ CF.CreateComplete
  , CF.CreateFailed
  , CF.DeleteComplete
  , CF.DeleteFailed
  , CF.RollbackComplete
  , CF.UpdateComplete
  , CF.UpdateFailed
  , CF.UpdateRollbackComplete
  ]

initialResourceStatus :: Set CF.ResourceStatus
initialResourceStatus =
  [ CF.CreateInProgress
  , CF.DeleteInProgress
  , CF.UpdateInProgress
  ]

isToken :: Token -> CF.StackEvent -> Bool
isToken token event = pure (toText token) == view CF.seClientRequestToken event
