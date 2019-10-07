module StackDeploy.Types where

import Data.Word (Word32)
import StackDeploy.InstanceSpec
import StackDeploy.Prelude

import qualified Data.ByteString.Builder          as BS
import qualified Data.ByteString.Lazy             as LBS
import qualified Data.Text.Encoding               as Text
import qualified System.Random                    as Random

newtype Id = Id Text
  deriving newtype ToText

data Operation
  = OpCreate InstanceSpec
  | OpDelete Id
  | OpUpdate Id InstanceSpec

data RemoteOperation = RemoteOperation
  { stackId   :: Id
  , token     :: Token
  }

data RemoteOperationResult = RemoteOperationFailure | RemoteOperationSuccess

newtype Token = Token Text
  deriving newtype ToText

verb :: Operation -> Text
verb = \case
  (OpCreate     _instanceSpec) -> "create"
  (OpDelete _id              ) -> "delete"
  (OpUpdate _id _instanceSpec) -> "update"

newToken :: forall m . MonadIO m => m Token
newToken = Token . text <$> bytes
  where
    text (wordA, wordB, wordC) = Text.decodeUtf8 . LBS.toStrict . BS.toLazyByteString
      $  "stack-deploy-"
      <> BS.word32HexFixed wordA
      <> BS.word32HexFixed wordB
      <> BS.word32HexFixed wordC

    bytes = (,,) <$> randomWord <*> randomWord <*> randomWord

    randomWord :: m Word32
    randomWord = liftIO Random.randomIO
