module StackDeploy.AWS (listResource) where

import Control.Lens (Lens', view)
import Control.Monad.Trans.AWS (AWSConstraint)
import Data.Conduit (ConduitT, (.|))
import Data.Conduit.Combinators (concatMap)
import Network.AWS (AWSPager, MonadAWS, Rs, paginate)

listResource
  :: (AWSConstraint r m, AWSPager a, MonadAWS m)
  => a
  -> Lens' (Rs a) [b]
  -> ConduitT () b m ()
listResource action getList = paginate action .| concatMap (view getList)
