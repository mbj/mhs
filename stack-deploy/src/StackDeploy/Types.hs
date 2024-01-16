module StackDeploy.Types where

import Data.Word (Word32)
import StackDeploy.InstanceSpec
import StackDeploy.Parameters
import StackDeploy.Prelude

import qualified Amazonka.CloudFormation.Types as CF
import qualified Data.ByteString.Builder       as BS
import qualified Data.ByteString.Lazy          as LBS
import qualified Data.Text.Encoding            as Text
import qualified System.Random                 as Random

type StackId = BoundText "StackDeploy.StackId"
type Token   = BoundText "StackDeploy.Token"

data ExistingStack = ExistingStack
  { outputs        :: [CF.Output]
  , parameterNames :: Set ParameterName
  , parameters     :: [CF.Parameter]
  , stack          :: CF.Stack
  , stackId        :: StackId
  }

data Operation env
  = OpCreate (InstanceSpec env) ParameterMap
  | OpDelete ExistingStack
  | OpUpdate ExistingStack (InstanceSpec env) ParameterMap

data RemoteOperation = RemoteOperation
  { stackId :: StackId
  , token   :: Token
  }

data RemoteOperationResult = RemoteOperationFailure | RemoteOperationSuccess

verb :: Operation env -> Text
verb = \case
  OpCreate{} -> "create"
  OpDelete{} -> "delete"
  OpUpdate{} -> "update"

newToken :: forall m . MonadIO m => m Token
newToken = convertImpure . text <$> bytes
  where
    text (wordA, wordB, wordC) = Text.decodeUtf8 . LBS.toStrict . BS.toLazyByteString
      $  "stack-deploy-"
      <> BS.word32HexFixed wordA
      <> BS.word32HexFixed wordB
      <> BS.word32HexFixed wordC

    bytes = (,,) <$> randomWord <*> randomWord <*> randomWord

    randomWord :: m Word32
    randomWord = liftIO Random.randomIO
