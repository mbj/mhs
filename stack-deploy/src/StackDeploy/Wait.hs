module StackDeploy.Wait (waitForAccept) where

import Data.Set (Set)
import StackDeploy.Events
import StackDeploy.Prelude
import StackDeploy.Types

import qualified Amazonka.CloudFormation.Types as CF
import qualified Data.Foldable                 as Foldable
import qualified MRIO.Amazonka                 as AWS

-- | Wait for remote operation to end in a final status
--
-- Apply action for each event related to the remote operation.
waitForAccept
  :: forall env . AWS.Env env
  => RemoteOperation
  -> (CF.StackEvent -> RIO env ())
  -> RIO env RemoteOperationResult
waitForAccept RemoteOperation{..} action =
  classify =<< pollEvents pollConfig action
  where
    classify :: Maybe CF.StackEvent -> RIO env RemoteOperationResult
    classify = maybe
      (throwString "No last event")
      assertPresentResourceStatus

    assertPresentResourceStatus
      = maybe
        (throwString "Last event has no resource status")
        remoteOperationResult
      . (.resourceStatus)

    pollConfig = (defaultPoll stackId)
      { eventFilter    = isToken token
      , startCondition = isStackEvent initialResourceStatus
      , stopCondition  = isStackEvent finalResourceStatus
      }

    remoteOperationResult :: CF.ResourceStatus -> RIO env RemoteOperationResult
    remoteOperationResult = \case
      CF.ResourceStatus_CREATE_COMPLETE          -> pure RemoteOperationSuccess
      CF.ResourceStatus_CREATE_FAILED            -> pure RemoteOperationFailure
      CF.ResourceStatus_DELETE_COMPLETE          -> pure RemoteOperationSuccess
      CF.ResourceStatus_DELETE_FAILED            -> pure RemoteOperationFailure
      CF.ResourceStatus_ROLLBACK_COMPLETE        -> pure RemoteOperationFailure
      CF.ResourceStatus_ROLLBACK_FAILED          -> pure RemoteOperationFailure
      CF.ResourceStatus_UPDATE_COMPLETE          -> pure RemoteOperationSuccess
      CF.ResourceStatus_UPDATE_FAILED            -> pure RemoteOperationFailure
      CF.ResourceStatus_UPDATE_ROLLBACK_COMPLETE -> pure RemoteOperationFailure
      status                    -> throwString $ message status
      where
        message :: Show a => a -> String
        message = ("Last event has unexpected resource status: " <>) . show

isStackEvent :: Set CF.ResourceStatus -> CF.StackEvent -> Bool
isStackEvent allowedStatus event = isStackEventType && isExpectedResourceStatus
  where
    isExpectedResourceStatus =
      event.resourceStatus `Foldable.elem` (pure <$> Foldable.toList allowedStatus)

    isStackEventType = event.resourceType == pure "AWS::CloudFormation::Stack"

finalResourceStatus :: Set CF.ResourceStatus
finalResourceStatus =
  [ CF.ResourceStatus_CREATE_COMPLETE
  , CF.ResourceStatus_CREATE_FAILED
  , CF.ResourceStatus_DELETE_COMPLETE
  , CF.ResourceStatus_DELETE_FAILED
  , CF.ResourceStatus_ROLLBACK_COMPLETE
  , CF.ResourceStatus_UPDATE_COMPLETE
  , CF.ResourceStatus_UPDATE_FAILED
  , CF.ResourceStatus_UPDATE_ROLLBACK_COMPLETE
  ]

initialResourceStatus :: Set CF.ResourceStatus
initialResourceStatus =
  [ CF.ResourceStatus_CREATE_IN_PROGRESS
  , CF.ResourceStatus_DELETE_IN_PROGRESS
  , CF.ResourceStatus_UPDATE_IN_PROGRESS
  ]

isToken :: Token -> CF.StackEvent -> Bool
isToken token event = pure (toText token) == event.clientRequestToken
