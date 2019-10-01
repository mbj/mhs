module AWS.Lambda.Runtime.S3
  ( TargetObject(..)
  , objectKeyText
  , syncTarget
  ) where

import AWS.Lambda.Runtime.Prelude
import Control.Monad (unless)
import Control.Monad.Catch (catchIf)
import Control.Monad.Trans.AWS (AWSConstraint)

import qualified Data.Text.IO              as Text
import qualified Network.AWS               as AWS
import qualified Network.AWS.Data.Body     as AWS
import qualified Network.AWS.S3.HeadObject as S3
import qualified Network.AWS.S3.PutObject  as S3
import qualified Network.AWS.S3.Types      as S3
import qualified Network.HTTP.Types        as HTTP

data TargetObject = TargetObject
  { bucketName :: S3.BucketName
  , message    :: Text
  , object     :: AWS.HashedBody
  , objectKey  :: S3.ObjectKey
  }

objectKeyText :: TargetObject -> Text
objectKeyText TargetObject{objectKey = S3.ObjectKey text} = text

syncTarget :: (AWSConstraint r m, AWS.MonadAWS m) => TargetObject -> m ()
syncTarget TargetObject{..} =
  putIfAbsent bucketName objectKey object (liftIO $ Text.putStrLn message)

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
