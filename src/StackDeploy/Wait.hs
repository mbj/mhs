module StackDeploy.Wait (waitForAccept) where

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
  :: forall env . HasAWS env
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

    assertPresentResourceStatus =
      maybe
        (throwString "Last event has no resource status")
        remoteOperationResult
        . view CF.seResourceStatus

    pollConfig = (defaultPoll stackId)
      { eventFilter    = isToken token
      , startCondition = isStackEvent initialResourceStatus
      , stopCondition  = isStackEvent finalResourceStatus
      }

    remoteOperationResult :: CF.ResourceStatus -> RIO env RemoteOperationResult
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
      status                    -> throwString $ message status
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
