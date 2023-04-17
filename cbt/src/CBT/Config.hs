module CBT.Config
  ( Config(..)
  , Env
  , askBackend
  , loadDefaultConfig
  , onDebug
  )
where


import CBT.Backend
import CBT.Prelude
import Control.Monad (when)
import Data.Maybe (isJust)

import qualified MIO.Log              as Log
import qualified UnliftIO.Environment as Environment

type Env env = (HasField "cbtConfig" env Config, Log.Env env)

data Config = Config
  { backend :: Backend
  , debug   :: Bool
  }

loadDefaultConfig :: MonadUnliftIO m => m Config
loadDefaultConfig = do
  backend <- loadBackend
  debug   <- isJust <$> Environment.lookupEnv "CBT_DEBUG"
  pure $ Config{..}

onDebug :: Env env => MIO env () -> MIO env ()
onDebug action = do
  Config{..} <- asks (.cbtConfig)
  when debug action

askBackend :: Env env => MIO env Backend
askBackend = asks (.cbtConfig.backend)
