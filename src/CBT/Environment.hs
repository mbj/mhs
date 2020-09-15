{-# LANGUAGE MonoLocalBinds #-}

module CBT.Environment
  ( Environment(..)
  , HasEnvironment
  , IOEnvironment
  , getEnvironment
  , newDefaultEnvironment
  , onDebug
  , runDefaultEnvironment
  , runEnvironment
  )
where

import CBT.IncrementalState
import CBT.Prelude
import CBT.Types
import Control.Monad.IO.Unlift (MonadUnliftIO(..), wrappedWithRunInIO)
import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Control.Monad.Trans.Class (MonadTrans(lift))
import Data.Maybe (isJust)

import qualified CBT.IncrementalState as IncrementalState
import qualified Colog
import qualified System.Environment   as Environment

type IOEnvironment = Environment (App IO)

data Environment m = Environment
  { debug     :: Bool
  , builds    :: IncrementalState ImageName
  , logAction :: Colog.LogAction m Colog.Message
  }

instance MonadIO m => Colog.HasLog (Environment m) Colog.Message m where
  getLogAction                        = logAction
  setLogAction logAction' environment = environment { logAction = logAction' }

newtype App m a = App { runApp :: ReaderT (Environment (App m)) m a }
  deriving newtype
    ( Applicative
    , Functor
    , Monad
    , MonadIO
    , MonadReader (Environment (App m))
    )

instance MonadTrans App where
  lift = App . lift

class (MonadUnliftIO m, MonadReader (Environment m) m, Colog.HasLog (Environment m) Colog.Message m) => HasEnvironment m where
  getEnvironment :: m (Environment m)

instance MonadUnliftIO m => MonadUnliftIO (App m) where
  withRunInIO = wrappedWithRunInIO App runApp

instance MonadUnliftIO m => HasEnvironment (App m) where
  getEnvironment = ask

newDefaultEnvironment :: MonadUnliftIO m => m (Environment (App m))
newDefaultEnvironment = do
  debug  <- isJust <$> liftIO (Environment.lookupEnv "CBT_DEBUG")
  builds <- IncrementalState.new
  pure $ Environment
    { logAction = Colog.richMessageAction
    , ..
    }

onDebug :: HasEnvironment m => m () -> m ()
onDebug action = do
  Environment{..} <- getEnvironment
  if debug
    then action
    else pure ()

runEnvironment :: Environment (App m) -> App m a -> m a
runEnvironment environment app = runReaderT (runApp app) environment

runDefaultEnvironment :: MonadUnliftIO m => App m a -> m a
runDefaultEnvironment app = do
  environment <- newDefaultEnvironment
  runEnvironment environment app
