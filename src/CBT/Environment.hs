{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS -Wno-orphans #-}

module CBT.Environment
  ( AppT
  , Environment(..)
  , HasEnvironment
  , WithEnv
  , getEnvironment
  , newDefaultEnvironment
  , onDebug
  , runDefaultEnvironment
  , runDefaultEnvironmentLog
  , runEnvironment
  , runEnvironmentLog
  )
where

import CBT.IncrementalState
import CBT.Prelude
import CBT.Types
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.IO.Unlift (MonadUnliftIO(..), wrappedWithRunInIO)
import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Data.Maybe (isJust)

import qualified CBT.IncrementalState as IncrementalState
import qualified Colog
import qualified System.Environment   as Environment

type WithEnv m env = (Colog.WithLog env Colog.Message m, HasEnvironment m, MonadUnliftIO m)

data Environment = Environment
  { debug  :: Bool
  , builds :: IncrementalState ImageName ImageBuildError ()
  }

newtype AppT m a = AppT { runAppT :: ReaderT Environment m a }
  deriving newtype
    ( Applicative
    , Functor
    , Monad
    , MonadCatch
    , MonadIO
    , MonadReader Environment
    , MonadThrow
    , MonadTrans
    )

deriving instance MonadCatch m => MonadCatch (Colog.LoggerT Colog.Message m)
deriving instance MonadThrow m => MonadThrow (Colog.LoggerT Colog.Message m)

instance MonadUnliftIO m => MonadUnliftIO (Colog.LoggerT Colog.Message m) where
  withRunInIO = wrappedWithRunInIO Colog.LoggerT Colog.runLoggerT

instance MonadUnliftIO m => MonadUnliftIO (AppT m) where
  withRunInIO = wrappedWithRunInIO AppT runAppT

class Monad m => HasEnvironment m where
  getEnvironment :: m Environment

instance Monad m => HasEnvironment (Colog.LoggerT Colog.Message (AppT m)) where
  getEnvironment = lift ask

instance Monad m => HasEnvironment (AppT m) where
  getEnvironment = ask

newDefaultEnvironment :: MonadUnliftIO m => m Environment
newDefaultEnvironment = do
  debug  <- isJust <$> liftIO (Environment.lookupEnv "CBT_DEBUG")
  builds <- IncrementalState.new
  pure $ Environment{..}

onDebug :: HasEnvironment m => m () -> m ()
onDebug action = do
  Environment{..} <- getEnvironment
  if debug
    then action
    else pure ()

runEnvironment
  :: Environment
  -> AppT m a
  -> m a
runEnvironment environment app = runReaderT (runAppT app) environment

runEnvironmentLog
  :: forall m a . MonadUnliftIO m
  => Environment
  -> Colog.LoggerT Colog.Message (AppT m) a
  -> m a
runEnvironmentLog environment app
  = runReaderT (runAppT $ Colog.usingLoggerT Colog.richMessageAction app) environment

runDefaultEnvironment
  :: MonadUnliftIO m
  => AppT m a
  -> m a
runDefaultEnvironment app = do
  environment <- newDefaultEnvironment
  runEnvironment environment app

runDefaultEnvironmentLog
  :: MonadUnliftIO m
  => Colog.LoggerT Colog.Message (AppT m) a
  -> m a
runDefaultEnvironmentLog
  = runDefaultEnvironment
  . Colog.usingLoggerT Colog.richMessageAction
