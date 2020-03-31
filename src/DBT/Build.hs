module DBT.Build (buildImage, masterUsername) where

import DBT.Image
import DBT.Path
import DBT.Prelude
import DBT.Process

import qualified DBT.Postgresql       as Postgresql
import qualified Data.ByteString.Lazy as LBS
import qualified System.Path          as Path
import qualified System.Process.Typed as Process

newtype ContainerName = ContainerName Text
  deriving newtype ToText

newtype Dockerfile = Dockerfile Text
  deriving newtype ToText

buildImage :: forall m . MonadIO m => m ()
buildImage = do
  containerName <- createContainer
  populateContainer containerName
  commitImage containerName
  where
    createContainer :: m ContainerName
    createContainer
      =   ContainerName
      <$> captureText (Process.proc "buildah" ["from", "alpine:3.11"])

    commitImage :: ContainerName -> m ()
    commitImage containerName
      = Process.runProcess_
      $ Process.proc "buildah"
      [ "commit"
      , "--"
      , convertText containerName
      , convertText imageName
      ]

populateContainer :: forall m . MonadIO m => ContainerName -> m ()
populateContainer containerName = do
  installPackages
  generatePassword
  initDB
  configureHBA
  where
    installPackages = do
      runContainer
        [ "apk"
        , "add"
        , "--no-cache"
        , "--"
        , "openssl"
        , "postgresql"
        ]

      runContainer
        [ "apk"
        , "add"
        , "--no-cache"
        , "--repository=http://dl-cdn.alpinelinux.org/alpine/edge/testing"
        , "--"
        , "daemontools"
        ]

    generatePassword
      = runContainerPostgres
      [ "openssl"
      , "rand"
      , "-base64"
      , "-out"
      , Path.toString pgMasterPasswordAbs
      , "32"
      ]

    initDB
      = runContainerPostgres
        [ "initdb"
        , "--auth=password"
        , "--data-checksums"
        , "--encoding=UTF8"
        , "--no-locale"
        , "--pwfile", Path.toString pgMasterPasswordAbs
        , "-D"
        , Path.toString pgData
        ]

    configureHBA :: m ()
    configureHBA
      = Process.runProcess_
      $ Process.setStdin (Process.byteStringInput $ LBS.fromStrict hbaSource)
      $ Process.proc
        "buildah"
        [ "run"
        , convertText containerName
        , "--"
        , "install"
        , "-o", "postgres"
        , "-g", "postgres"
        , "-m", "0400"
        , "/dev/stdin"
        , Path.toString pgHBAConfAbs
        ]

    runContainerPostgres :: [String] -> m ()
    runContainerPostgres command
      = runContainer $ ["setuidgid", convertText masterUsername] <> command

    runContainer :: [String] -> m ()
    runContainer command
      = Process.runProcess_
      . Process.proc "buildah"
      $ ["run", "--", convertText containerName] <> command

masterUsername :: Postgresql.Username
masterUsername = Postgresql.Username "postgres"
