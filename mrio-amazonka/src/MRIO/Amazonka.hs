{-# OPTIONS -Wno-orphans #-}

module MRIO.Amazonka
  ( Env
  , HasResourceMap(..)
  , ResourceMap
  , paginate
  , paginateEither
  , send
  , sendEither
  , withResourceMap
  )
where

import Control.Monad ((=<<))
import Control.Monad.IO.Unlift (MonadUnliftIO, liftIO, withRunInIO)
import Control.Monad.Reader (asks)
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Trans.Resource.Internal (MonadResource, ReleaseMap, ResourceT(..), liftResourceT)
import Data.Conduit (ConduitT)
import Data.Either (Either)
import Data.Function (($), (.))
import Data.IORef (IORef)
import GHC.Records (HasField, getField)
import MRIO.Core

import qualified Amazonka

type ResourceMap = IORef ReleaseMap

class HasResourceMap env where
  resourceMap :: env -> ResourceMap

instance HasResourceMap env => MonadResource (RIO env) where
  liftResourceT (ResourceT run) = (liftIO . run) =<< asks resourceMap

type Env env =
  ( MonadResource (RIO env)
  , HasResourceMap env
  , HasField "awsEnv" env Amazonka.Env
  )

paginate
  :: (Amazonka.AWSPager a, Env env)
  => a
  -> ConduitT () (Amazonka.AWSResponse a) (RIO env) ()
paginate pager = do
  env <- asks $ getField @"awsEnv"
  Amazonka.paginate env pager

paginateEither
  :: (Amazonka.AWSPager a, Env env)
  => a
  -> ConduitT () (Amazonka.AWSResponse a) (RIO env) (Either Amazonka.Error ())
paginateEither pager = do
  env <- asks $ getField @"awsEnv"
  Amazonka.paginateEither env pager

send
  :: (Amazonka.AWSRequest a, Env env)
  => a
  -> RIO env (Amazonka.AWSResponse a)
send request = do
  env <- asks $ getField @"awsEnv"
  Amazonka.send env request

sendEither
  :: (Amazonka.AWSRequest a, Env env)
  => a
  -> RIO env (Either Amazonka.Error (Amazonka.AWSResponse a))
sendEither request = do
  env <- asks $ getField @"awsEnv"
  Amazonka.sendEither env request

withResourceMap :: MonadUnliftIO m => (ResourceMap -> m a) -> m a
withResourceMap action =
  withRunInIO $ \run -> runResourceT $ ResourceT $ run . action
