module StackDeploy.Types where

import Control.Monad.Trans.AWS (AWSConstraint)
import Data.ByteString.Builder (toLazyByteString, word32HexFixed)
import Data.ByteString.Lazy (toStrict)
import Data.Text.Encoding (decodeUtf8)
import Data.Word (Word32)
import Network.AWS (MonadAWS)
import Network.AWS.CloudFormation.Types (Capability, Parameter)
import StackDeploy.Prelude
import Stratosphere (Template)
import System.Random (randomIO)

newtype Id = Id Text
  deriving newtype ToText

newtype Name = Name Text
  deriving newtype ToText
  deriving stock   Eq

newtype RoleARN = RoleARN Text
  deriving newtype ToText
  deriving stock   Eq

data Operation
  = OpCreate Name InstanceSpec Template
  | OpDelete Id
  | OpUpdate Id InstanceSpec Template

data InstanceSpec = InstanceSpec
  { capabilities :: [Capability]
  , onSuccess    :: forall m r . (AWSConstraint r m, MonadAWS m) => m ()
  , parameters   :: [Parameter]
  , prepareSync  :: forall m r . (AWSConstraint r m, MonadAWS m) => m ()
  , roleARN      :: Maybe RoleARN
  }

data RemoteOperation = RemoteOperation
  { stackId   :: Id
  , token     :: Token
  }

data RemoteOperationResult = RemoteOperationFailure | RemoteOperationSuccess

newtype Token = Token Text
  deriving newtype ToText

verb :: Operation -> Text
verb = \case
  (OpCreate _name _instanceSpec _template) -> "create"
  (OpDelete _id                          ) -> "delete"
  (OpUpdate _id   _instanceSpec _template) -> "update"

newToken :: forall m . MonadIO m => m Token
newToken = Token . text <$> bytes
  where
    text (wordA, wordB, wordC) = decodeUtf8 . toStrict . toLazyByteString
      $  "stack-deploy-"
      <> word32HexFixed wordA
      <> word32HexFixed wordB
      <> word32HexFixed wordC

    bytes = (,,) <$> randomWord <*> randomWord <*> randomWord

    randomWord :: m Word32
    randomWord = liftIO randomIO
