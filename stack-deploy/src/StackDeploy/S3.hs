module StackDeploy.S3
  ( TargetObject(..)
  , hashedTargetObject
  , syncTarget
  , targetObjectKeyText
  )
where

import Control.Monad (unless)
import StackDeploy.Prelude

import qualified Data.ByteString           as BS
import qualified Data.Text.Encoding        as Text
import qualified MRIO.Amazonka             as AWS
import qualified Network.AWS.Data.Body     as AWS
import qualified Network.AWS.S3.HeadObject as S3
import qualified Network.AWS.S3.PutObject  as S3
import qualified Network.AWS.S3.Types      as S3
import qualified Network.HTTP.Types        as HTTP
import qualified StackDeploy.AWS           as AWS

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
targetObjectKeyText = objectKeyText . objectKey

testObjectExists
  :: AWS.Env env
  => S3.BucketName
  -> S3.ObjectKey
  -> RIO env Bool
testObjectExists bucketName objectKey =
  catchJust handleNotFoundError
    ((void . AWS.send $ S3.headObject bucketName objectKey) >> pure True)
    pure
  where
    handleNotFoundError :: AWS.Error -> Maybe Bool
    handleNotFoundError
      ( AWS.ServiceError
        AWS.ServiceError'
        { _serviceStatus = HTTP.Status { HTTP.statusCode = 404 } }
      )
      = pure False
    handleNotFoundError _error = empty

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
    void . AWS.send $ S3.putObject bucketName objectKey (AWS.Hashed object)

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
