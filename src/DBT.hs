module DBT
  ( Backend.buildDefinition
  , imageName
  , populateDatabaseImage
  , startDatabaseContainer
  , stopDatabaseContainer
  , withDatabaseContainer
  , withDatabaseContainerImage
  , withDatabaseContainerProcess
  , withDatabaseContainerProcessRun_
  )
where

import DBT.Prelude

import qualified CBT
import qualified CBT.Backend
import qualified DBT.Backend          as Backend
import qualified DBT.Postgresql       as Postgresql
import qualified System.Process.Typed as Process

imageName :: CBT.ImageName
imageName = getField @"imageName" Backend.buildDefinition

withDatabaseContainer
  :: CBT.WithEnv m env
  => CBT.Prefix
  -> (Postgresql.ClientConfig -> m a)
  -> m a
withDatabaseContainer prefix action = do
  containerName <- CBT.nextContainerName prefix
  CBT.getImplementation >>= \case
    CBT.Docker -> Backend.withDatabaseContainer @'CBT.Docker containerName action
    CBT.Podman -> Backend.withDatabaseContainer @'CBT.Podman containerName action

withDatabaseContainerProcess
  :: CBT.WithEnv m env
  => CBT.Prefix
  -> Process.ProcessConfig stdin stdout stderr
  -> (Process.ProcessConfig stdin stdout stderr -> (Process.Process stdin stdout stderr -> m a) -> m a)
  -> (Process.Process stdin stdout stderr -> m a)
  -> m a
withDatabaseContainerProcess prefix proc withProcess action = do
  withDatabaseContainer prefix $ \clientConfig -> do
    env <- Postgresql.getEnv clientConfig
    withProcess (Process.setEnv env proc) action

withDatabaseContainerProcessRun_
  :: CBT.WithEnv m env
  => CBT.Prefix
  -> Process.ProcessConfig stdin stdout stderr
  -> m ()
withDatabaseContainerProcessRun_ prefix proc =
  withDatabaseContainerProcess prefix proc Process.withProcessWait_ Process.checkExitCode

populateDatabaseImage
  :: CBT.WithEnv m env
  => CBT.Prefix
  -> CBT.ImageName
  -> (Postgresql.ClientConfig -> m ())
  -> m (Either CBT.ImageBuildError ())
populateDatabaseImage prefix imageName' action = do
  containerName <- CBT.nextContainerName prefix
  CBT.getImplementation >>= \case
    CBT.Docker -> Backend.populateDatabaseImage @'CBT.Docker containerName imageName' action
    CBT.Podman -> Backend.populateDatabaseImage @'CBT.Podman containerName imageName' action

withDatabaseContainerImage
  :: CBT.WithEnv m env
  => CBT.Prefix
  -> CBT.ImageName
  -> (Postgresql.ClientConfig -> m a)
  -> m a
withDatabaseContainerImage prefix imageName' action = do
  containerName <- CBT.nextContainerName prefix
  CBT.getImplementation >>= \case
    CBT.Docker -> Backend.withDatabaseContainerImage @'CBT.Docker containerName imageName' action
    CBT.Podman -> Backend.withDatabaseContainerImage @'CBT.Podman containerName imageName' action

startDatabaseContainer
  :: CBT.WithEnv m env
  => CBT.Prefix
  -> m (CBT.ContainerName, Postgresql.ClientConfig)
startDatabaseContainer prefix = do
  containerName <- CBT.nextContainerName prefix
  config <- CBT.getImplementation >>= \case
    CBT.Docker -> Backend.startDatabaseContainer @'CBT.Docker containerName
    CBT.Podman -> Backend.startDatabaseContainer @'CBT.Podman containerName
  pure (containerName, config)

stopDatabaseContainer
  :: CBT.WithEnv m env
  => CBT.ContainerName
  -> m ()
stopDatabaseContainer containerName = do
  CBT.getImplementation >>= \case
    CBT.Docker -> CBT.Backend.stop @'CBT.Docker containerName
    CBT.Podman -> CBT.Backend.stop @'CBT.Podman containerName
