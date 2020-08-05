module StackDeploy.AWS
  ( MonadAWS
  , listResource
  , withAWS
  )
where

import Control.Lens (set)
import Control.Monad.Trans.AWS (runAWST)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Conduit (ConduitT, (.|))
import Data.Conduit.Combinators (concatMap)
import Network.AWS (AWS, AWSPager, MonadAWS, Rs, paginate)
import Network.AWS.Lens
import StackDeploy.Prelude

import qualified Network.AWS as AWS
import qualified System.IO   as IO

listResource
  :: (AWSPager a, MonadAWS m)
  => a
  -> Lens' (Rs a) [b]
  -> ConduitT () b m ()
listResource action getList = paginate action .| concatMap (view getList)

withAWS :: (MonadCatch m, MonadUnliftIO m) => AWS a -> m a
withAWS action = do
  logger <- AWS.newLogger AWS.Info IO.stderr
  env    <- AWS.newEnv AWS.Discover <&> set AWS.envLogger logger

  liftIO . runResourceT $ runAWST env action
