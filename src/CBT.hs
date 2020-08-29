{-# LANGUAGE RankNTypes #-}

module CBT
  ( module CBT.BuildDefinition
  , module CBT.Types
  , build
  , buildIfAbsent
  , buildRun
  , getImplementation
  , nextContainerName
  , printInspect
  , printLogs
  , readContainerFile
  , removeContainer
  , withContainer
  )
where

import CBT.Backend (Backend)
import CBT.BuildDefinition
import CBT.Environment
import CBT.Prelude
import CBT.Types

import qualified CBT.Backend
import qualified Data.ByteString    as BS
import qualified Data.UUID.V4       as UUID
import qualified System.Environment as Environment
import qualified System.Path        as Path

withContainer
  :: HasEnvironment m
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
  :: HasEnvironment m
  => BuildDefinition
  -> m ()
build buildDefinition = do
  implementation <- getImplementation
  case implementation of
    Docker -> CBT.Backend.build @'Docker buildDefinition
    Podman -> CBT.Backend.build @'Podman buildDefinition

buildRun
  :: HasEnvironment m
  => BuildDefinition
  -> ContainerDefinition
  -> m ()
buildRun buildDefinition containerDefinition = do
  implementation <- getImplementation
  case implementation of
    Docker -> CBT.Backend.buildRun @'Docker buildDefinition containerDefinition
    Podman -> CBT.Backend.buildRun @'Podman buildDefinition containerDefinition

buildIfAbsent
  :: HasEnvironment m
  => BuildDefinition
  -> m ()
buildIfAbsent buildDefinition = do
  implementation <- getImplementation
  case implementation of
    Docker -> CBT.Backend.buildIfAbsent @'Docker buildDefinition
    Podman -> CBT.Backend.buildIfAbsent @'Podman buildDefinition

readContainerFile
  :: HasEnvironment m
  => ContainerName
  -> Path.AbsFile
  -> m BS.ByteString
readContainerFile containerName path = do
  implementation <- getImplementation
  case implementation of
    Docker -> CBT.Backend.readContainerFile @'Docker containerName path
    Podman -> CBT.Backend.readContainerFile @'Podman containerName path

removeContainer
  :: HasEnvironment m
  => ContainerName
  -> m ()
removeContainer containerName = do
  implementation <- getImplementation
  case implementation of
    Docker -> CBT.Backend.removeContainer @'Docker containerName
    Podman -> CBT.Backend.removeContainer @'Podman containerName

printLogs
  :: HasEnvironment m
  => ContainerName
  -> m ()
printLogs containerName = do
  implementation <- getImplementation
  case implementation of
    Docker -> CBT.Backend.printLogs @'Docker containerName
    Podman -> CBT.Backend.printLogs @'Podman containerName

printInspect
  :: HasEnvironment m
  => ContainerName
  -> m ()
printInspect containerName = do
  implementation <- getImplementation
  case implementation of
    Docker -> CBT.Backend.printInspect @'Docker containerName
    Podman -> CBT.Backend.printInspect @'Podman containerName

getImplementation :: forall m . HasEnvironment m => m Implementation
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

try
  :: forall (b :: Implementation) m . (Backend b, HasEnvironment m)
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
