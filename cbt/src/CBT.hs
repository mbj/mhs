module CBT
  ( CBT.Config
  , CBT.Env
  , CBT.loadDefaultConfig
  , Environment(..)
  , runDefaultEnvironment
  )
where

import CBT.Prelude

import qualified CBT.Config as CBT
import qualified MIO.Log    as Log

data Environment = Environment
  { cbtConfig :: CBT.Config
  , logAction :: Log.Action
  }

runDefaultEnvironment
  :: MonadUnliftIO m
  => MIO Environment a
  -> m a
runDefaultEnvironment action = do
  cbtConfig <- CBT.loadDefaultConfig
  runMIO Environment{logAction = Log.defaultCLIAction, ..} action

