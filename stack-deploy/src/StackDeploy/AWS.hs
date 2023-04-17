module StackDeploy.AWS (listResource) where

import Data.Conduit (ConduitT, (.|))
import Data.Conduit.Combinators (concatMap)
-- import Network.AWS.Types as Exports
import StackDeploy.Prelude

import qualified Amazonka
import qualified MIO.Amazonka as AWS

listResource
  :: (AWS.Env env, Amazonka.AWSPager a)
  => a
  -> (Amazonka.AWSResponse a -> [b])
  -> ConduitT () b (MIO env) ()
listResource action map = AWS.paginate action .| concatMap map
