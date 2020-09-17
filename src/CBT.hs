{-# LANGUAGE RankNTypes #-}

module CBT
  ( module CBT.BuildDefinition
  , module CBT.Environment
  , module CBT.Types
  , build
  , buildIfAbsent
  , buildRun
  , commit
  , getImplementation
  , nextContainerName
  , printInspect
  , printLogs
  , readContainerFile
  , removeContainer
  , runLockedBuild
  , runLockedBuildThrow
  , runReadStdout
  , withContainer
  )
where

import CBT.Backend (Backend)
import CBT.BuildDefinition
import CBT.Environment
import CBT.Prelude
import CBT.Types

import qualified CBT.Backend
import qualified CBT.IncrementalState
import qualified Data.ByteString    as BS
import qualified Data.UUID.V4       as UUID
import qualified System.Environment as Environment
import qualified System.Path        as Path
import qualified UnliftIO.Exception as Exception

withContainer
  :: WithEnv m env
  => BuildDefinition
  -> ContainerDefinition
  -> m a
  -> m a
withContainer buildDefinition containerDefinition action = do
  implementation <- getImplementation
  case implementation of
    Docker -> CBT.Backend.withContainer @'Docker buildDefinition containerDefinition action
    Podman -> CBT.Backend.withContainer @'Podman buildDefinition containerDefinition action

build
  :: WithEnv m env
  => BuildDefinition
  -> m ()
build buildDefinition = do
  implementation <- getImplementation
  case implementation of
    Docker -> CBT.Backend.build @'Docker buildDefinition
    Podman -> CBT.Backend.build @'Podman buildDefinition

buildRun
  :: WithEnv m env
  => BuildDefinition
  -> ContainerDefinition
  -> m ()
buildRun buildDefinition containerDefinition = do
  implementation <- getImplementation
  case implementation of
    Docker -> CBT.Backend.buildRun @'Docker buildDefinition containerDefinition
    Podman -> CBT.Backend.buildRun @'Podman buildDefinition containerDefinition

runReadStdout
  :: WithEnv m env
  => ContainerDefinition
  -> m BS.ByteString
runReadStdout containerDefinition = do
  implementation <- getImplementation
  case implementation of
    Docker -> CBT.Backend.runReadStdout @'Docker containerDefinition
    Podman -> CBT.Backend.runReadStdout @'Podman containerDefinition

buildIfAbsent
  :: WithEnv m env
  => BuildDefinition
  -> m ()
buildIfAbsent buildDefinition = do
  implementation <- getImplementation
  case implementation of
    Docker -> CBT.Backend.buildIfAbsent @'Docker buildDefinition
    Podman -> CBT.Backend.buildIfAbsent @'Podman buildDefinition

readContainerFile
  :: WithEnv m env
  => ContainerName
  -> Path.AbsFile
  -> m BS.ByteString
readContainerFile containerName path = do
  implementation <- getImplementation
  case implementation of
    Docker -> CBT.Backend.readContainerFile @'Docker containerName path
    Podman -> CBT.Backend.readContainerFile @'Podman containerName path

removeContainer
  :: WithEnv m env
  => ContainerName
  -> m ()
removeContainer containerName = do
  implementation <- getImplementation
  case implementation of
    Docker -> CBT.Backend.removeContainer @'Docker containerName
    Podman -> CBT.Backend.removeContainer @'Podman containerName

commit
  :: WithEnv m env
  => ContainerName
  -> ImageName
  -> m ()
commit containerName imageName = do
  implementation <- getImplementation
  case implementation of
    Docker -> CBT.Backend.commit @'Docker containerName imageName
    Podman -> CBT.Backend.commit @'Podman containerName imageName

printLogs
  :: WithEnv m env
  => ContainerName
  -> m ()
printLogs containerName = do
  implementation <- getImplementation
  case implementation of
    Docker -> CBT.Backend.printLogs @'Docker containerName
    Podman -> CBT.Backend.printLogs @'Podman containerName

printInspect
  :: WithEnv m env
  => ContainerName
  -> m ()
printInspect containerName = do
  implementation <- getImplementation
  case implementation of
    Docker -> CBT.Backend.printInspect @'Docker containerName
    Podman -> CBT.Backend.printInspect @'Podman containerName

getImplementation :: forall m env . WithEnv m env => m Implementation
getImplementation =
  maybe discover fromEnv =<< liftIO (Environment.lookupEnv "CBT_BACKEND")
  where
    fromEnv :: String -> m Implementation
    fromEnv = \case
      "docker" -> pure Docker
      "podman" -> pure Podman
      other    -> liftIO . fail $ "Unknown CBT_BACKEND: " <> show other

    discover :: m Implementation
    discover = do
      podman <- try @'Podman Podman
      docker <- try @'Docker Docker

      maybe
        (liftIO $ fail "Neither found podman nor docker backend")
        pure
        (podman <|> docker)

runLockedBuild
  :: WithEnv m env
  => ImageName
  -> m (Either ImageBuildError ())
  -> m (Either ImageBuildError ())
runLockedBuild imageName buildAction = do
  Environment{..} <- getEnvironment
  CBT.IncrementalState.runBuild builds imageName buildAction

runLockedBuildThrow
  :: WithEnv m env
  => ImageName
  -> m (Either ImageBuildError ())
  -> m ()
runLockedBuildThrow image action = either Exception.throwIO pure =<< runLockedBuild image action

try
  :: forall (b :: Implementation) m env . (Backend b, WithEnv m env)
  => Implementation
  -> m (Maybe Implementation)
try implementation = do
  isAvailable <- CBT.Backend.available @b
  if isAvailable
    then pure $ pure implementation
    else pure empty

nextContainerName :: MonadIO m => Prefix -> m ContainerName
nextContainerName prefix = do
  uuid <- liftIO UUID.nextRandom
  pure $ ContainerName $ toText prefix <> "-" <> convertText (show uuid)
