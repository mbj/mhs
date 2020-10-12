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
  :: CBT.WithEnv env
  => CBT.Prefix
  -> (Postgresql.ClientConfig -> RIO env a)
  -> RIO env a
withDatabaseContainer prefix action = do
  containerName <- CBT.nextContainerName prefix
  CBT.getImplementation >>= \case
    CBT.Docker -> Backend.withDatabaseContainer @'CBT.Docker containerName action
    CBT.Podman -> Backend.withDatabaseContainer @'CBT.Podman containerName action

withDatabaseContainerProcess
  :: CBT.WithEnv env
  => CBT.Prefix
  -> Process.ProcessConfig stdin stdout stderr
  -> (Process.ProcessConfig stdin stdout stderr -> (Process.Process stdin stdout stderr -> RIO env a) -> RIO env a)
  -> (Process.Process stdin stdout stderr -> RIO env a)
  -> RIO env a
withDatabaseContainerProcess prefix proc withProcess action = do
  withDatabaseContainer prefix $ \clientConfig -> do
    env <- Postgresql.getEnv clientConfig
    withProcess (Process.setEnv env proc) action

withDatabaseContainerProcessRun_
  :: CBT.WithEnv env
  => CBT.Prefix
  -> Process.ProcessConfig stdin stdout stderr
  -> RIO env ()
withDatabaseContainerProcessRun_ prefix proc =
  withDatabaseContainerProcess prefix proc Process.withProcessWait_ Process.checkExitCode

populateDatabaseImage
  :: CBT.WithEnv env
  => CBT.Prefix
  -> CBT.ImageName
  -> (Postgresql.ClientConfig -> RIO env ())
  -> RIO env (Either CBT.ImageBuildError ())
populateDatabaseImage prefix imageName' action = do
  containerName <- CBT.nextContainerName prefix
  CBT.getImplementation >>= \case
    CBT.Docker -> Backend.populateDatabaseImage @'CBT.Docker containerName imageName' action
    CBT.Podman -> Backend.populateDatabaseImage @'CBT.Podman containerName imageName' action

withDatabaseContainerImage
  :: CBT.WithEnv env
  => CBT.Prefix
  -> CBT.ImageName
  -> (Postgresql.ClientConfig -> RIO env a)
  -> RIO env a
withDatabaseContainerImage prefix imageName' action = do
  containerName <- CBT.nextContainerName prefix
  CBT.getImplementation >>= \case
    CBT.Docker -> Backend.withDatabaseContainerImage @'CBT.Docker containerName imageName' action
    CBT.Podman -> Backend.withDatabaseContainerImage @'CBT.Podman containerName imageName' action

startDatabaseContainer
  :: CBT.WithEnv env
  => CBT.Prefix
  -> RIO env (CBT.ContainerName, Postgresql.ClientConfig)
startDatabaseContainer prefix = do
  containerName <- CBT.nextContainerName prefix
  config <- CBT.getImplementation >>= \case
    CBT.Docker -> Backend.startDatabaseContainer @'CBT.Docker containerName
    CBT.Podman -> Backend.startDatabaseContainer @'CBT.Podman containerName
  pure (containerName, config)

stopDatabaseContainer
  :: CBT.WithEnv env
  => CBT.ContainerName
  -> RIO env ()
stopDatabaseContainer containerName = do
  CBT.getImplementation >>= \case
    CBT.Docker -> CBT.Backend.stop @'CBT.Docker containerName
    CBT.Podman -> CBT.Backend.stop @'CBT.Podman containerName
