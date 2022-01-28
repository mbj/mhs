module CBT.Environment
  ( Environment(..)
  , HasEnvironment
  , WithEnv
  , getEnvironment
  , newDefaultEnvironment
  , onDebug
  , runDefaultEnvironment
  )
where

import CBT.IncrementalState
import CBT.Prelude
import CBT.Types
import Data.Maybe (isJust)

import qualified CBT.IncrementalState as IncrementalState
import qualified MRIO.Log             as Log
import qualified System.Environment   as Environment

type WithEnv env = (HasEnvironment env, Log.Env env)

data Environment = Environment
  { debug     :: Bool
  , builds    :: IncrementalState ImageName ImageBuildError ()
  , logAction :: Log.Action
  }

class HasEnvironment env where
  getEnvironment :: env -> Environment

instance HasEnvironment Environment where
  getEnvironment = identity

newDefaultEnvironment :: MonadUnliftIO m => m Environment
newDefaultEnvironment = do
  debug  <- isJust <$> liftIO (Environment.lookupEnv "CBT_DEBUG")
  builds <- IncrementalState.new
  pure $ Environment
    { logAction = Log.defaultCLIAction
    , ..
    }

onDebug :: HasEnvironment env => RIO env () -> RIO env ()
onDebug action = do
  Environment{..} <- getEnvironment <$> ask
  if debug
    then action
    else pure ()

runDefaultEnvironment
  :: MonadUnliftIO m
  => RIO Environment a
  -> m a
runDefaultEnvironment app = do
  environment <- newDefaultEnvironment
  runRIO environment app
