module DBT.Backend (Backend(..)) where

import DBT.Prelude
import System.Path ((</>))

import qualified CBT
import qualified CBT.Backend
import qualified CBT.Backend        as CBT (Backend)
import qualified CBT.TH
import qualified DBT.Postgresql     as Postgresql
import qualified DBT.Wait           as Wait
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
  getClientConfig containerName =
    mkClientConfig
      <$> getHostPort @b containerName
      <*> getMasterPassword @b containerName

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
    CBT.withContainer buildDefinition (containerDefinition containerName) $ do
      hostPort <- getHostPort       @b containerName
      password <- getMasterPassword @b containerName

      waitForPort containerName hostPort

      action $ mkClientConfig hostPort password

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

mkClientConfig
  :: Postgresql.HostPort
  -> Postgresql.Password
  -> Postgresql.ClientConfig
mkClientConfig hostPort password =
  Postgresql.ClientConfig
    { databaseName = Postgresql.DatabaseName "postgres"
    , hostName     = localhost
    , hostPort     = pure hostPort
    , password     = pure password
    , sslMode      = empty
    , sslRootCert  = empty
    , userName     = masterUserName
    , ..
    }

waitForPort :: MonadIO m => CBT.ContainerName -> Postgresql.HostPort -> m ()
waitForPort containerName hostPort
  = Wait.wait
  $ Wait.Config
  { prefix      = "[DBT]"
  , maxAttempts = 100
  , waitTime    = 100000  -- 100ms
  , printStatus = debug
  , hostName    = localhost
  , onFail      = CBT.printLogs containerName
  , ..
  }

buildDefinition :: CBT.BuildDefinition
buildDefinition = $$(CBT.TH.readDockerfile (CBT.Prefix "cbt") (Path.file "Dockerfile"))

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
