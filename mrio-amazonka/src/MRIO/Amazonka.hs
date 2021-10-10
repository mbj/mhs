{-# OPTIONS -Wno-orphans #-}

module MRIO.Amazonka
  ( Env
  , EnvConfig(..)
  , HasResourceMap(..)
  , ResourceMap
  , paginate
  , runAWS
  , send
  , withResourceMap
  )
where

import Control.Monad ((=<<))
import Control.Monad.IO.Unlift (MonadUnliftIO, liftIO, withRunInIO)
import Control.Monad.Reader (asks)
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Trans.Resource.Internal (MonadResource, ReleaseMap, ResourceT(..), liftResourceT)
import Data.Conduit (ConduitT, transPipe)
import Data.Function (($), (.))
import Data.IORef (IORef)
import MRIO.Core

import qualified Network.AWS as AWS

type ResourceMap = IORef ReleaseMap

class HasResourceMap env where
  resourceMap :: env -> ResourceMap

class EnvConfig env where
  awsEnv :: env -> AWS.Env

instance HasResourceMap env => MonadResource (RIO env) where
  liftResourceT (ResourceT run) = (liftIO . run) =<< asks resourceMap

type Env env = (MonadResource (RIO env), EnvConfig env, HasResourceMap env)

runAWS
  :: Env env
  => AWS.AWS a
  -> RIO env a
runAWS action = do
  env <- asks awsEnv
  AWS.runAWS env action

paginate
  :: (AWS.AWSPager a, Env env)
  => a
  -> ConduitT () (AWS.Rs a) (RIO env) ()
paginate pager = transPipe runAWS (AWS.paginate pager)

send
  :: (AWS.AWSRequest a, Env env)
  => a
  -> RIO env (AWS.Rs a)
send request = runAWS $ AWS.send request

withResourceMap :: MonadUnliftIO m => (ResourceMap -> m a) -> m a
withResourceMap action =
  withRunInIO $ \run -> runResourceT $ ResourceT $ run . action
