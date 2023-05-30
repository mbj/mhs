{-# OPTIONS -Wno-orphans #-}

module MIO.Amazonka
  ( Env
  , HasResourceMap(..)
  , ResourceMap
  , Transaction
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
import Data.Typeable (Typeable)
import GHC.Records (HasField)
import MIO.Core

import qualified Amazonka

type ResourceMap = IORef ReleaseMap

type Transaction a = (Typeable a, Typeable (Amazonka.AWSResponse a))

class HasResourceMap env where
  resourceMap :: env -> ResourceMap

instance HasResourceMap env => MonadResource (MIO env) where
  liftResourceT (ResourceT run) = (liftIO . run) =<< asks resourceMap

type Env env =
  ( MonadResource (MIO env)
  , HasResourceMap env
  , HasField "awsEnv" env Amazonka.Env
  )

paginate
  :: (Amazonka.AWSPager a, Env env, Transaction a)
  => a
  -> ConduitT () (Amazonka.AWSResponse a) (MIO env) ()
paginate pager = do
  env <- asks (.awsEnv)
  Amazonka.paginate env pager

paginateEither
  :: (Amazonka.AWSPager a, Env env, Transaction a)
  => a
  -> ConduitT () (Amazonka.AWSResponse a) (MIO env) (Either Amazonka.Error ())
paginateEither pager = do
  env <- asks (.awsEnv)
  Amazonka.paginateEither env pager

send
  :: (Amazonka.AWSRequest a, Env env, Transaction a)
  => a
  -> MIO env (Amazonka.AWSResponse a)
send request = do
  env <- asks (.awsEnv)
  Amazonka.send env request

sendEither
  :: (Amazonka.AWSRequest a, Env env, Transaction a)
  => a
  -> MIO env (Either Amazonka.Error (Amazonka.AWSResponse a))
sendEither request = do
  env <- asks (.awsEnv)
  Amazonka.sendEither env request

withResourceMap :: MonadUnliftIO m => (ResourceMap -> m a) -> m a
withResourceMap action =
  withRunInIO $ \run -> runResourceT $ ResourceT $ run . action
