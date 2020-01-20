module AWS.Lambda.Runtime.Deploy (byteStringTargetObject, podmanTargetObject) where

import AWS.Lambda.Runtime.Prelude
import Data.Text.Encoding (decodeUtf8)

import qualified AWS.Lambda.Runtime.Podman as Podman
import qualified AWS.Lambda.Runtime.S3     as S3
import qualified AWS.Lambda.Runtime.Zip    as Zip
import qualified Network.AWS               as AWS
import qualified Network.AWS.Data.Body     as AWS
import qualified Network.AWS.S3.Types      as S3

podmanTargetObject
  :: forall m . MonadIO m
  => Podman.Config
  -> S3.BucketName
  -> m S3.TargetObject
podmanTargetObject config bucketName =
  byteStringTargetObject bucketName <$> Podman.build config

byteStringTargetObject
  :: S3.BucketName
  -> Podman.Executable
  -> S3.TargetObject
byteStringTargetObject bucketName (Podman.Executable bootstrap) =
  S3.TargetObject
    { message = "Uploading new lambda function: " <> objectKeyText
    , ..
    }
  where
    object        = AWS.toHashed $ Zip.mkZip bootstrap
    objectKeyText = decodeUtf8 (AWS.sha256Base16 object) <> ".zip"
    objectKey     = S3.ObjectKey objectKeyText
