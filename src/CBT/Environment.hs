module CBT.Environment
  ( Environment(..)
  , HasEnvironment(..)
  , WithLog
  , defaultEnvironment
  , logDebug
  , logError
  , logInfo
  , runDefaultEnvironment
  , runEnvironment
  )
where

import CBT.Prelude
import Control.Monad.IO.Unlift (MonadUnliftIO(..), wrappedWithRunInIO)
import Control.Monad.Reader (MonadReader, MonadTrans, ReaderT, ask, runReaderT)
import Data.Maybe (isJust)
import GHC.Stack (HasCallStack, callStack, withFrozenCallStack)

import qualified Colog
import qualified System.Environment as Environment

data Environment = Environment
  { logAction :: forall m . MonadIO m => Colog.LogAction m Colog.Message
  , debug     :: Bool
  }

type WithLog m = (HasEnvironment m, HasCallStack)

class MonadUnliftIO m => HasEnvironment m where
  getEnvironment :: m Environment

defaultEnvironment :: Environment
defaultEnvironment = Environment
  { debug     = False
  , logAction = Colog.richMessageAction
  }

newtype AppT m a = App { runAppT :: ReaderT Environment m a }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader Environment
    , MonadTrans
    )

instance MonadUnliftIO m => MonadUnliftIO (AppT m) where
  withRunInIO = wrappedWithRunInIO App runAppT

instance MonadUnliftIO m => HasEnvironment (AppT m) where
  getEnvironment = ask

runEnvironment :: forall m a . MonadIO m => Environment -> AppT m a -> m a
runEnvironment environment action =
  runReaderT (runAppT action) =<< addDebug
  where
    addDebug :: m Environment
    addDebug = do
      debug <- isJust <$> liftIO (Environment.lookupEnv "CBT_DEBUG")
      pure $ environment { debug = debug }

runDefaultEnvironment :: MonadIO m => AppT m a -> m a
runDefaultEnvironment = runEnvironment defaultEnvironment

logMessage :: HasEnvironment m => Colog.Message -> m ()
logMessage message = do
  Environment{..} <- getEnvironment
  runLogAction (Colog.cfilter ((||) debug . (/=) Colog.Debug . Colog.msgSeverity) logAction) message

runLogAction :: Colog.LogAction m message -> message -> m ()
runLogAction (Colog.LogAction action) = action

logM :: forall a m . (WithLog m, ToText a) => Colog.Severity -> a -> m ()
logM msgSeverity (toText -> msgText) =
  withFrozenCallStack (logMessage Colog.Msg { msgStack = callStack, .. })

logDebug :: forall a m . (WithLog m, ToText a)  => a -> m ()
logDebug = withFrozenCallStack (logM Colog.Debug)

logError :: forall a m . (WithLog m, ToText a) => a -> m ()
logError = withFrozenCallStack (logM Colog.Error)

logInfo :: forall a m . (WithLog m, ToText a) => a -> m ()
logInfo = withFrozenCallStack (logM Colog.Info)
