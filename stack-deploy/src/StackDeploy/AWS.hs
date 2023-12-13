module StackDeploy.AWS (nestedResourceC) where

import Data.Conduit (ConduitT, (.|))
import Data.Conduit.Combinators (concatMap)
import StackDeploy.Prelude

import qualified Amazonka
import qualified MIO.Amazonka as AWS

-- | Convert paginator with a nested list item into a paginator of that item
nestedResourceC
  :: (AWS.Env env, Amazonka.AWSPager a, AWS.Transaction a)
  => a
  -> (Amazonka.AWSResponse a -> [b])
  -> ConduitT () b (MIO env) ()
nestedResourceC action map = AWS.paginate action .| concatMap map
