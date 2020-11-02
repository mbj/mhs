{-# LANGUAGE RankNTypes #-}

module CBT
  ( module CBT.BuildDefinition
  , module CBT.Environment
  , module CBT.Types
  , build
  , buildIfAbsent
  , buildRun
  , commit
  , getHostPort
  , getImplementation
  , login
  , nextContainerName
  , printInspect
  , printLogs
  , push
  , readContainerFile
  , removeContainer
  , runLockedBuild
  , runLockedBuildThrow
  , runReadStdout
  , stop
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
  :: WithEnv env
  => BuildDefinition
  -> ContainerDefinition
  -> RIO env a
  -> RIO env a
withContainer buildDefinition containerDefinition action = do
  implementation <- getImplementation
  case implementation of
    Docker -> CBT.Backend.withContainer @'Docker buildDefinition containerDefinition action
    Podman -> CBT.Backend.withContainer @'Podman buildDefinition containerDefinition action

build
  :: WithEnv env
  => BuildDefinition
  -> RIO env ()
build buildDefinition = do
  implementation <- getImplementation
  case implementation of
    Docker -> CBT.Backend.build @'Docker buildDefinition
    Podman -> CBT.Backend.build @'Podman buildDefinition

buildRun
  :: WithEnv env
  => BuildDefinition
  -> ContainerDefinition
  -> RIO env ()
buildRun buildDefinition containerDefinition = do
  implementation <- getImplementation
  case implementation of
    Docker -> CBT.Backend.buildRun @'Docker buildDefinition containerDefinition
    Podman -> CBT.Backend.buildRun @'Podman buildDefinition containerDefinition

runReadStdout
  :: WithEnv env
  => ContainerDefinition
  -> RIO env BS.ByteString
runReadStdout containerDefinition = do
  implementation <- getImplementation
  case implementation of
    Docker -> CBT.Backend.runReadStdout @'Docker containerDefinition
    Podman -> CBT.Backend.runReadStdout @'Podman containerDefinition

buildIfAbsent
  :: WithEnv env
  => BuildDefinition
  -> RIO env ()
buildIfAbsent buildDefinition = do
  implementation <- getImplementation
  case implementation of
    Docker -> CBT.Backend.buildIfAbsent @'Docker buildDefinition
    Podman -> CBT.Backend.buildIfAbsent @'Podman buildDefinition

readContainerFile
  :: WithEnv env
  => ContainerName
  -> Path.AbsFile
  -> RIO env BS.ByteString
readContainerFile containerName path = do
  implementation <- getImplementation
  case implementation of
    Docker -> CBT.Backend.readContainerFile @'Docker containerName path
    Podman -> CBT.Backend.readContainerFile @'Podman containerName path

removeContainer
  :: WithEnv env
  => ContainerName
  -> RIO env ()
removeContainer containerName = do
  implementation <- getImplementation
  case implementation of
    Docker -> CBT.Backend.removeContainer @'Docker containerName
    Podman -> CBT.Backend.removeContainer @'Podman containerName

commit
  :: WithEnv env
  => ContainerName
  -> ImageName
  -> RIO env ()
commit containerName imageName = do
  implementation <- getImplementation
  case implementation of
    Docker -> CBT.Backend.commit @'Docker containerName imageName
    Podman -> CBT.Backend.commit @'Podman containerName imageName

stop
  :: WithEnv env
  => ContainerName
  -> RIO env ()
stop containerName = do
  implementation <- getImplementation
  case implementation of
    Docker -> CBT.Backend.stop @'Docker containerName
    Podman -> CBT.Backend.stop @'Podman containerName

getHostPort
  :: WithEnv env
  => ContainerName
  -> Port
  -> RIO env Port
getHostPort containerName port = do
  implementation <- getImplementation
  case implementation of
    Docker -> CBT.Backend.getHostPort @'Docker containerName port
    Podman -> CBT.Backend.getHostPort @'Podman containerName port

printLogs
  :: WithEnv env
  => ContainerName
  -> RIO env ()
printLogs containerName = do
  implementation <- getImplementation
  case implementation of
    Docker -> CBT.Backend.printLogs @'Docker containerName
    Podman -> CBT.Backend.printLogs @'Podman containerName

printInspect
  :: WithEnv env
  => ContainerName
  -> RIO env ()
printInspect containerName = do
  implementation <- getImplementation
  case implementation of
    Docker -> CBT.Backend.printInspect @'Docker containerName
    Podman -> CBT.Backend.printInspect @'Podman containerName

login
  :: WithEnv env
  => Registry
  -> Username
  -> Password
  -> RIO env ()
login registry username password = do
  implementation <- getImplementation
  case implementation of
    Docker -> CBT.Backend.login @'Docker registry username password
    Podman -> CBT.Backend.login @'Podman registry username password

push
  :: WithEnv env
  => ImageName
  -> Destination
  -> RIO env ()
push imageName destination = do
  implementation <- getImplementation
  case implementation of
    Docker -> CBT.Backend.push @'Docker imageName destination
    Podman -> CBT.Backend.push @'Podman imageName destination

getImplementation :: forall env . RIO env Implementation
getImplementation =
  maybe discover fromEnv =<< liftIO (Environment.lookupEnv "CBT_BACKEND")
  where
    fromEnv :: String -> RIO env Implementation
    fromEnv = \case
      "docker" -> pure Docker
      "podman" -> pure Podman
      other    -> liftIO . fail $ "Unknown CBT_BACKEND: " <> show other

    discover :: RIO env Implementation
    discover = do
      podman <- try @'Podman Podman
      docker <- try @'Docker Docker

      maybe
        (liftIO $ fail "Neither found podman nor docker backend")
        pure
        (podman <|> docker)

runLockedBuild
  :: WithEnv env
  => ImageName
  -> RIO env (Either ImageBuildError ())
  -> RIO env (Either ImageBuildError ())
runLockedBuild imageName buildAction = do
  Environment{..} <- getEnvironment <$> ask
  CBT.IncrementalState.runBuild builds imageName buildAction

runLockedBuildThrow
  :: WithEnv env
  => ImageName
  -> RIO env (Either ImageBuildError ())
  -> RIO env ()
runLockedBuildThrow image action = either Exception.throwIO pure =<< runLockedBuild image action

try
  :: forall (b :: Implementation) env . Backend b
  => Implementation
  -> RIO env (Maybe Implementation)
try implementation = do
  isAvailable <- CBT.Backend.available @b
  if isAvailable
    then pure $ pure implementation
    else pure empty

nextContainerName :: Prefix -> RIO env ContainerName
nextContainerName prefix = do
  uuid <- liftIO UUID.nextRandom
  pure $ ContainerName $ toText prefix <> "-" <> convertText (show uuid)
