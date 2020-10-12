module StackDeploy.Environment where

import StackDeploy.Prelude

import qualified Network.AWS.S3.Types as S3

newtype Environment = Environment
  { getTemplateBucketName :: forall env . Maybe (RIO env S3.BucketName)
  }

class HasEnvironment env where
  getEnvironment :: env -> Environment

instance HasEnvironment Environment where
  getEnvironment = identity

defaultEnvironment :: Environment
defaultEnvironment = Environment
  { getTemplateBucketName = empty
  }
