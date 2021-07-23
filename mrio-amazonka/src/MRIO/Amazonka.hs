{-# OPTIONS -Wno-orphans #-}

module MRIO.Amazonka
  ( AWS.Env
  , HasAWS
  , HasAWSEnv(..)
  , HasResourceMap(..)
  , ResourceMap
  , paginate
  , runAWS
  , send
  , withResourceMap
  )
where

import           Control.Monad                         ( (=<<) )
import           Control.Monad.IO.Unlift               ( MonadUnliftIO
                                                       , liftIO
                                                       , withRunInIO
                                                       )
import           Control.Monad.Reader                  ( asks )
import           Control.Monad.Trans.Resource          ( runResourceT )
import           Control.Monad.Trans.Resource.Internal ( MonadResource
                                                       , ReleaseMap
                                                       , ResourceT(..)
                                                       , liftResourceT
                                                       )
import           Data.Conduit                          ( ConduitT
                                                       , transPipe
                                                       )
import           Data.Function                         ( ($), (.) )
import           Data.IORef                            ( IORef )
import           MRIO.Core

import qualified Network.AWS as AWS

class HasAWSEnv env where
  awsEnv :: env -> AWS.Env

type ResourceMap = IORef ReleaseMap

class HasResourceMap env where
  resourceMap :: env -> ResourceMap

instance HasResourceMap env => MonadResource (RIO env) where
  liftResourceT (ResourceT run) = (liftIO . run) =<< asks resourceMap

type HasAWS env = (MonadResource (RIO env), HasAWSEnv env)

runAWS
  :: HasAWS env
  => AWS.AWS a
  -> RIO env a
runAWS action = do
  env <- asks awsEnv
  AWS.runAWS env action

paginate
  :: (AWS.AWSPager a, HasAWS env)
  => a
  -> ConduitT () (AWS.Rs a) (RIO env) ()
paginate pager = transPipe runAWS (AWS.paginate pager)

send
  :: (AWS.AWSRequest a, HasAWS env)
  => a
  -> RIO env (AWS.Rs a)
send request = runAWS $ AWS.send request

withResourceMap :: MonadUnliftIO m => (ResourceMap -> m a) -> m a
withResourceMap action =
  withRunInIO $ \run -> runResourceT $ ResourceT $ run . action
