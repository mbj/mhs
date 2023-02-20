module StackDeploy.S3
  ( TargetObject(..)
  , hashedTargetObject
  , syncTarget
  , targetObjectKeyText
  )
where

import Control.Monad (unless)
import StackDeploy.Prelude

import qualified Amazonka.Data.Body     as AWS
import qualified Amazonka.S3.HeadObject as S3
import qualified Amazonka.S3.PutObject  as S3
import qualified Amazonka.S3.Types      as S3
import qualified Amazonka.Types         as AWS
import qualified Data.ByteString        as BS
import qualified Data.Text.Encoding     as Text
import qualified MRIO.Amazonka          as AWS
import qualified Network.HTTP.Types     as HTTP

data TargetObject = TargetObject
  { uploadCallback :: forall m . MonadIO m => Text -> m ()
  , bucketName     :: S3.BucketName
  , object         :: AWS.HashedBody
  , objectKey      :: S3.ObjectKey
  }

syncTarget :: AWS.Env env => TargetObject -> RIO env ()
syncTarget TargetObject{..} =
  putIfAbsent bucketName objectKey object (uploadCallback $ objectKeyText objectKey)

targetObjectKeyText :: TargetObject -> Text
targetObjectKeyText = objectKeyText . (.objectKey)

testObjectExists
  :: forall env . AWS.Env env
  => S3.BucketName
  -> S3.ObjectKey
  -> RIO env Bool
testObjectExists bucketName objectKey =
  either withError (const $ pure True) =<< AWS.sendEither (S3.newHeadObject bucketName objectKey)
  where
    withError :: AWS.Error -> RIO env Bool
    withError = \case
      (AWS.ServiceError AWS.ServiceError' { _serviceErrorStatus = HTTP.Status { HTTP.statusCode = 404 } }) ->
         pure False
      error -> throwM error

putIfAbsent
  :: AWS.Env env
  => S3.BucketName
  -> S3.ObjectKey
  -> AWS.HashedBody
  -> RIO env ()
  -> RIO env ()
putIfAbsent bucketName objectKey object callback = do
  exists <- testObjectExists bucketName objectKey

  unless exists $ do
    callback
    void . AWS.send $ S3.newPutObject bucketName objectKey (AWS.Hashed object)

hashedTargetObject
  :: S3.BucketName
  -> Text
  -> Text
  -> BS.ByteString
  -> TargetObject
hashedTargetObject bucketName prefix ext body =
  TargetObject
    { objectKey      = S3.ObjectKey $ prefix <> "-" <> Text.decodeUtf8 (AWS.sha256Base16 object) <> "." <> ext
    , uploadCallback = const $ pure ()
    , ..
    }
  where
    object= AWS.toHashed body

objectKeyText :: S3.ObjectKey -> Text
objectKeyText (S3.ObjectKey text) = text
