{-# LANGUAGE AllowAmbiguousTypes #-}
module DBT where

import DBT.Prelude

import qualified CBT
import qualified CBT.Backend
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

startDatabaseContainer
  :: MonadIO m
  => CBT.Prefix
  -> m (CBT.ContainerName, Postgresql.ClientConfig)
startDatabaseContainer prefix = do
  containerName  <- CBT.nextContainerName prefix
  implementation <- CBT.getImplementation
  config <- case implementation of
    CBT.Docker -> Backend.startDatabaseContainer @'CBT.Docker containerName
    CBT.Podman -> Backend.startDatabaseContainer @'CBT.Podman containerName
  pure (containerName, config)

stopDatabaseContainer
  :: MonadIO m
  => CBT.ContainerName
  -> m ()
stopDatabaseContainer containerName = do
  implementation <- CBT.getImplementation
  case implementation of
    CBT.Docker -> CBT.Backend.stop @'CBT.Docker containerName
    CBT.Podman -> CBT.Backend.stop @'CBT.Podman containerName

withDatabaseEnv
  :: MonadUnliftIO m
  => CBT.Prefix
  -> Process.ProcessConfig () () ()
  -> m ()
withDatabaseEnv prefix proc =
  withDatabaseContainer prefix $ \config -> do
    environment <- liftIO Environment.getEnvironment
    Process.runProcess_ $ Process.setEnv (environment <> Postgresql.toEnv config) proc

