module AWS.Lambda.Runtime.Deploy
  ( Config(..)
  , ExecutablePath(..)
  , ImageName(..)
  , PackageName(..)
  , TargetName(..)
  , TargetObject(..)
  , getFunctionTarget
  , syncTarget
  )
where

import AWS.Lambda.Runtime.Prelude
import Control.Monad (unless)
import Control.Monad.Catch (catchIf)
import Control.Monad.Trans.AWS (AWSConstraint)
import Data.Text.Encoding (decodeUtf8)
import System.FilePath (FilePath)

import qualified AWS.Lambda.Runtime.Podman as Podman
import qualified AWS.Lambda.Runtime.Zip    as Zip
import qualified Data.Text.IO              as Text
import qualified Network.AWS               as AWS
import qualified Network.AWS.Data.Body     as AWS
import qualified Network.AWS.S3.HeadObject as S3
import qualified Network.AWS.S3.PutObject  as S3
import qualified Network.AWS.S3.Types      as S3
import qualified Network.HTTP.Types        as HTTP

newtype ExecutablePath = ExecutablePath FilePath
  deriving newtype ToText

newtype ImageName = ImageName Text
  deriving newtype ToText

newtype PackageName = PackageName Text
  deriving newtype ToText

newtype TargetName = TargetName Text
  deriving newtype ToText

data Config = Config
  { bucketName     :: S3.BucketName
  , executablePath :: ExecutablePath
  , packageName    :: PackageName
  , targetName     :: TargetName
  }

data TargetObject = TargetObject
  { bucketName    :: S3.BucketName
  , message       :: Text
  , object        :: AWS.HashedBody
  , objectKey     :: S3.ObjectKey
  , objectKeyText :: Text
  }

syncTarget :: (AWSConstraint r m, AWS.MonadAWS m) => TargetObject -> m ()
syncTarget TargetObject{..} =
  putIfAbsent bucketName objectKey object (liftIO $ Text.putStrLn message)

getFunctionTarget :: forall m . MonadIO m => Podman.Config -> S3.BucketName -> m TargetObject
getFunctionTarget config bucketName = do
  bootstrap <- Podman.build config

  let
    object        = AWS.toHashed $ Zip.mkZip bootstrap
    objectKeyText = decodeUtf8 (AWS.sha256Base16 object) <> ".zip"
    objectKey     = S3.ObjectKey objectKeyText

  pure TargetObject
    { message = "Uploading new lambda function: " <> objectKeyText
    , ..
    }

testObjectExists
  :: (AWSConstraint r m, AWS.MonadAWS m)
  => S3.BucketName
  -> S3.ObjectKey
  -> m Bool
testObjectExists bucketName objectKey =
  catchIf isNotFoundError
    ((void . AWS.send $ S3.headObject bucketName objectKey) >> pure True)
    (const $ pure False)
  where
    isNotFoundError
      ( AWS.ServiceError
        AWS.ServiceError'
        { _serviceStatus = HTTP.Status { HTTP.statusCode = 404 } }
      )
      = True

    isNotFoundError _ = False

putIfAbsent
  :: (AWSConstraint r m, AWS.MonadAWS m)
  => S3.BucketName
  -> S3.ObjectKey
  -> AWS.HashedBody
  -> m ()
  -> m ()
putIfAbsent bucketName objectKey object callback = do
  exists <- testObjectExists bucketName objectKey

  unless exists $ do
    callback
    void . AWS.send $ S3.putObject bucketName objectKey (AWS.Hashed object)
