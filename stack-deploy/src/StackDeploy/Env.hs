module StackDeploy.Env (Config(..), Env, defaultConfig) where

import GHC.Records (HasField)
import StackDeploy.Prelude

import qualified Network.AWS.S3.Types as S3

newtype Config env = Config
  { getTemplateBucketName :: Maybe (RIO env S3.BucketName)
  }

type Env env = HasField "stackDeployConfig" env (Config env)

defaultConfig :: Config env
defaultConfig = Config
  { getTemplateBucketName = empty
  }
