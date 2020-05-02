module DBT.Backend (Backend(..)) where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import DBT.Prelude
import System.Path ((</>))

import qualified CBT
import qualified CBT.Backend
import qualified CBT.Backend        as CBT (Backend)
import qualified CBT.TH
import qualified DBT.Postgresql     as Postgresql
import qualified Data.Text          as Text
import qualified Data.Text.Encoding as Text
import qualified System.Path        as Path

class CBT.Backend b => Backend b where
  getHostPort :: MonadIO m => CBT.ContainerName -> m Postgresql.HostPort
  getHostPort containerName
    =   Postgresql.HostPort
    .   CBT.unPort
    <$> CBT.Backend.getHostPort @b containerName containerPort

  getMasterPassword :: MonadIO m => CBT.ContainerName -> m Postgresql.Password
  getMasterPassword containerName =
    Postgresql.Password . rstrip . Text.decodeUtf8 <$>
      CBT.Backend.readContainerFile @b containerName pgMasterPasswordAbs
    where
      rstrip = Text.dropWhileEnd (== '\n')

  getClientConfig
    :: MonadIO m
    => CBT.ContainerName
    -> m Postgresql.ClientConfig
  getClientConfig containerName = do
    hostPort <- pure <$> getHostPort @b containerName
    password <- pure <$> getMasterPassword @b containerName

    pure Postgresql.ClientConfig
      { databaseName = Postgresql.DatabaseName "postgres"
      , hostName     = localhost
      , sslMode      = empty
      , sslRootCert  = empty
      , userName     = masterUserName
      , ..
      }

  startDatabaseContainer
    :: MonadIO m
    => CBT.ContainerName
    -> m Postgresql.ClientConfig
  startDatabaseContainer containerName = do
    CBT.buildRun buildDefinition (containerDefinition containerName)
    getClientConfig @b containerName

  withDatabaseContainer
    :: MonadUnliftIO m
    => CBT.ContainerName
    -> (Postgresql.ClientConfig -> m a)
    -> m a
  withDatabaseContainer containerName action =
    CBT.withContainer buildDefinition (containerDefinition containerName) $
      action =<< getClientConfig @b containerName

instance Backend 'CBT.Docker
instance Backend 'CBT.Podman

containerPort :: CBT.Port
containerPort = CBT.Port 5432

localhost :: Postgresql.HostName
localhost = Postgresql.HostName "127.0.0.1"

pgData :: Path.AbsDir
pgData = pgHome </> Path.relDir "data"

pgHome :: Path.AbsDir
pgHome = Path.absDir "/var/lib/postgresql"

pgMasterPasswordAbs :: Path.AbsFile
pgMasterPasswordAbs = pgHome </> Path.relFile "master-password.txt"

masterUserName :: Postgresql.UserName
masterUserName = Postgresql.UserName "postgres"

#ifndef __HLINT__
buildDefinition :: CBT.BuildDefinition
buildDefinition = $$(CBT.TH.readDockerfile (CBT.Prefix "cbt") (Path.file "Dockerfile"))
#endif

postgresqlDefinition
  :: CBT.ContainerName
  -> [String]
  -> CBT.ContainerDefinition
postgresqlDefinition containerName arguments =
  CBT.ContainerDefinition
    { detach           = CBT.Foreground
    , imageName        = (CBT.imageName :: CBT.BuildDefinition -> CBT.ImageName) buildDefinition
    , mounts           = []
    , programArguments = convertText masterUserName : arguments
    , programName      = "setuidgid"
    , publishPorts     = []
    , remove           = CBT.Remove
    , removeOnRunFail  = CBT.Remove
    , workDir          = pgHome
    , ..
    }

containerDefinition :: CBT.ContainerName -> CBT.ContainerDefinition
containerDefinition containerName
  = deamonize
  $ postgresqlDefinition
    containerName
    [ "postgres"
    , "-D", Path.toString pgData
    , "-h", "0.0.0.0"  -- connections from outside the container
    , "-k", ""         -- no unix socket
    ]
  where
    deamonize value = value
      { CBT.detach          = CBT.Detach
      , CBT.remove          = CBT.Remove
      , CBT.removeOnRunFail = CBT.Remove
      , CBT.publishPorts    = [CBT.Port 5432]
      }
