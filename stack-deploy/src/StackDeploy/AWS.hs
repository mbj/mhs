module StackDeploy.AWS
  ( module Exports
  , AWS.paginate
  , AWS.send
  , listResource
  )
where

import Data.Conduit (ConduitT, (.|))
import Data.Conduit.Combinators (concatMap)
import Network.AWS (AWSPager)
import Network.AWS.Lens
import Network.AWS.Types as Exports
import StackDeploy.Prelude

import qualified MRIO.Amazonka as AWS

listResource
  :: (AWS.Env env, AWSPager a)
  => a
  -> Lens' (Rs a) [b]
  -> ConduitT () b (RIO env) ()
listResource action getList = AWS.paginate action .| concatMap (view getList)
