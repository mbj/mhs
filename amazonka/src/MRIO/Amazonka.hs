module MRIO.Amazonka
  ( HasAWS(..)
  , paginate
  , send
  )
where

import           Data.Conduit                   ( ConduitT
                                                , transPipe
                                                )
import           Data.Function                  ( ($) )
import           MRIO.Core
import           Network.AWS                    ( AWS
                                                , AWSPager
                                                , AWSRequest
                                                , Rs
                                                )

import qualified Network.AWS                   as AWS

class HasAWS env where
  runAWS :: AWS a -> RIO env a

paginate :: (AWSPager a, HasAWS env) => a -> ConduitT () (Rs a) (RIO env) ()
paginate pager = transPipe runAWS (AWS.paginate pager)

send :: (AWSRequest a, HasAWS env) => a -> RIO env (Rs a)
send request = runAWS $ AWS.send request
