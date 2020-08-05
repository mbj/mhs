module StackDeploy.Environment where

import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Control.Monad.Trans.Resource
import StackDeploy.AWS
import StackDeploy.Prelude

import qualified Network.AWS.S3.Types as S3

newtype Environment = Environment
  { getTemplateBucketName :: forall m . MonadAWS m => Maybe (m S3.BucketName)
  }

newtype EnvT m a = EnvT (ReaderT Environment m a)
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadAWS
    , MonadCatch
    , MonadIO
    , MonadReader Environment
    , MonadResource
    , MonadThrow
    )

class MonadAWS m => StackDeployEnv m where
  getEnvironment :: m Environment

instance MonadAWS m => StackDeployEnv (EnvT m) where
  getEnvironment = ask

runEnvironment :: Environment -> EnvT m a -> m a
runEnvironment environment (EnvT reader) = runReaderT reader environment

defaultEnvironment :: Environment
defaultEnvironment = Environment
  { getTemplateBucketName = empty
  }
