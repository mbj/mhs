module AWS.Lambda.Runtime.Deploy (getFunctionTarget) where

import AWS.Lambda.Runtime.Prelude
import Data.Text.Encoding (decodeUtf8)

import qualified AWS.Lambda.Runtime.Podman as Podman
import qualified AWS.Lambda.Runtime.S3     as S3
import qualified AWS.Lambda.Runtime.Zip    as Zip
import qualified Network.AWS               as AWS
import qualified Network.AWS.Data.Body     as AWS
import qualified Network.AWS.S3.Types      as S3

getFunctionTarget :: forall m . MonadIO m => Podman.Config -> S3.BucketName -> m S3.TargetObject
getFunctionTarget config bucketName = do
  bootstrap <- Podman.build config

  let
    object        = AWS.toHashed $ Zip.mkZip bootstrap
    objectKeyText = decodeUtf8 (AWS.sha256Base16 object) <> ".zip"
    objectKey     = S3.ObjectKey objectKeyText

  pure S3.TargetObject
    { message = "Uploading new lambda function: " <> objectKeyText
    , ..
    }
