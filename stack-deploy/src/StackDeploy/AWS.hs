module StackDeploy.AWS
  ( module Exports
  , MRIO.Amazonka.paginate
  , MRIO.Amazonka.send
  , listResource
  )
where

import Data.Conduit (ConduitT, (.|))
import Data.Conduit.Combinators (concatMap)
import Network.AWS (AWSPager)
import Network.AWS.Lens
import Network.AWS.Types as Exports
import StackDeploy.Prelude

import qualified MRIO.Amazonka

listResource
  :: (AWSPager a, HasAWS env)
  => a
  -> Lens' (Rs a) [b]
  -> ConduitT () b (RIO env) ()
listResource action getList = MRIO.Amazonka.paginate action .| concatMap (view getList)
