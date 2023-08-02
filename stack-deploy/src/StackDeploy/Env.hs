module StackDeploy.Env (Config(..), Env, defaultConfig) where

import StackDeploy.Prelude

import qualified Amazonka.S3.Types as S3

newtype Config env = Config
  { getTemplateBucketName :: Maybe (MIO env S3.BucketName)
  }

type Env env = HasField "stackDeployConfig" env (Config env)

defaultConfig :: Config env
defaultConfig = Config
  { getTemplateBucketName = empty
  }
