module StackDeploy.Config where

import StackDeploy.Prelude

import qualified Network.AWS.S3.Types as S3

newtype Config env = Config
  { getTemplateBucketName :: Maybe (RIO env S3.BucketName)
  }

class HasConfig env where
  getConfig :: env -> Config env

defaultConfig :: Config env
defaultConfig = Config
  { getTemplateBucketName = empty
  }
