{-# LANGUAGE AllowAmbiguousTypes #-}
module DBT where

import DBT.Prelude

import qualified CBT
import qualified DBT.Backend          as Backend
import qualified DBT.Postgresql       as Postgresql
import qualified System.Environment   as Environment
import qualified System.Process.Typed as Process

withDatabaseContainer
  :: MonadUnliftIO m
  => CBT.Prefix
  -> (Postgresql.ClientConfig -> m a)
  -> m a
withDatabaseContainer prefix action = do
  containerName  <- CBT.nextContainerName prefix
  implementation <- CBT.getImplementation
  case implementation of
    CBT.Docker -> Backend.withDatabaseContainer @'CBT.Docker containerName action
    CBT.Podman -> Backend.withDatabaseContainer @'CBT.Podman containerName action

withDatabaseEnv
  :: MonadUnliftIO m
  => CBT.Prefix
  -> Process.ProcessConfig () () ()
  -> m ()
withDatabaseEnv prefix proc =
  withDatabaseContainer prefix $ \config -> do
    environment <- liftIO Environment.getEnvironment
    Process.runProcess_ $ Process.setEnv (environment <> Postgresql.toEnv config) proc

