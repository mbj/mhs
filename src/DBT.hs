module DBT
  ( Backend.buildDefinition
  , imageName
  , populateDatabaseImage
  , startDatabaseContainer
  , stopDatabaseContainer
  , withDatabaseContainer
  , withDatabaseContainerImage
  )
where

import DBT.Prelude

import qualified CBT
import qualified CBT.Backend
import qualified DBT.Backend     as Backend
import qualified DBT.Postgresql  as Postgresql

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
