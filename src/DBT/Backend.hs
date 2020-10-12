module DBT.Backend
  ( buildDefinition
  , populateDatabaseImage
  , startDatabaseContainer
  , withDatabaseContainer
  , withDatabaseContainerImage
  )
where

import Control.Arrow (left)
import DBT.Prelude
import System.Path ((</>))

import qualified CBT.Backend
import qualified CBT.Backend        as CBT
import qualified CBT.Environment    as CBT
import qualified CBT.TH
import qualified CBT.Types          as CBT
import qualified DBT.Postgresql     as Postgresql
import qualified DBT.Wait           as Wait
import qualified Data.Text          as Text
import qualified Data.Text.Encoding as Text
import qualified System.Path        as Path
import qualified UnliftIO.Exception as Exception

getHostPort
  :: forall b env . (CBT.Backend b, CBT.WithEnv env)
  => CBT.ContainerName
  -> RIO env Postgresql.HostPort
getHostPort containerName
  =   Postgresql.HostPort
  .   CBT.unPort
  <$> CBT.Backend.getHostPort @b containerName containerPort

getMasterPassword
  :: forall b env . (CBT.Backend b, CBT.WithEnv env)
  => CBT.ContainerName
  -> RIO env Postgresql.Password
getMasterPassword containerName =
  Postgresql.Password . rstrip . Text.decodeUtf8 <$>
    CBT.Backend.readContainerFile @b containerName pgMasterPasswordAbs
  where
    rstrip = Text.dropWhileEnd (== '\n')

getClientConfig
  :: forall b env . (CBT.Backend b, CBT.WithEnv env)
  => CBT.ContainerName
  -> RIO env Postgresql.ClientConfig
getClientConfig containerName =
  mkClientConfig
    <$> getHostPort @b containerName
    <*> getMasterPassword @b containerName

startDatabaseContainer
  :: forall b env . (CBT.Backend b, CBT.WithEnv env)
  => CBT.ContainerName
  -> RIO env Postgresql.ClientConfig
startDatabaseContainer containerName = do
  CBT.buildRun @b buildDefinition (containerDefinition containerName)
  getClientConfig @b containerName

populateDatabaseImage
  :: forall b env . (CBT.Backend b, CBT.WithEnv env)
  => CBT.ContainerName
  -> CBT.ImageName
  -> (Postgresql.ClientConfig -> RIO env ())
  -> RIO env (Either CBT.ImageBuildError ())
populateDatabaseImage containerName imageName action =
   fmap toError . Exception.tryAnyDeep $ do
     Exception.bracket_
       (CBT.buildRun @b buildDefinition containerDefinition')
       (CBT.removeContainer @b containerName)
       run
  where
    containerDefinition' = (containerDefinition containerName)
      { CBT.remove = CBT.NoRemove
      }

    toError :: Either Exception.SomeException a -> Either CBT.ImageBuildError a
    toError = left (CBT.ImageBuildError . convert . show)

    run :: RIO env ()
    run = do
      runAction @b containerName action
      CBT.stop @b containerName
      CBT.commit @b containerName imageName

withDatabaseContainer
  :: forall b env a . (CBT.Backend b, CBT.WithEnv env)
  => CBT.ContainerName
  -> (Postgresql.ClientConfig -> RIO env a)
  -> RIO env a
withDatabaseContainer containerName
  = CBT.withContainer @b buildDefinition (containerDefinition containerName)
  . runAction @b containerName

withDatabaseContainerImage
  :: forall b env a . (CBT.Backend b, CBT.WithEnv env)
  => CBT.ContainerName
  -> CBT.ImageName
  -> (Postgresql.ClientConfig -> RIO env a)
  -> RIO env a
withDatabaseContainerImage containerName imageName
  = CBT.withContainerDefinition @b containerDefinition'
  . runAction @b containerName
  where
    containerDefinition' :: CBT.ContainerDefinition
    containerDefinition' = (containerDefinition containerName) { CBT.imageName = imageName }

runAction
  :: forall b env a . (CBT.Backend b, CBT.WithEnv env)
  => CBT.ContainerName
  -> (Postgresql.ClientConfig -> RIO env a)
  -> RIO env a
runAction containerName action = do
  hostPort <- getHostPort       @b containerName
  password <- getMasterPassword @b containerName

  let config = mkClientConfig hostPort password

  waitForPort @b containerName config

  action config

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

waitForPort
  :: forall b env . (CBT.Backend b, CBT.WithEnv env)
  => CBT.ContainerName
  -> Postgresql.ClientConfig
  -> RIO env ()
waitForPort containerName clientConfig
  = Wait.wait
  $ Wait.Config
  { prefix      = "[DBT]"
  , maxAttempts = 100
  , waitTime    = 100000  -- 100ms
  , onFail      = CBT.printInspect @b containerName >> CBT.printLogs @b containerName
  , ..
  }

buildDefinition :: CBT.BuildDefinition
buildDefinition = $$(CBT.TH.readDockerfile (CBT.Prefix "dbt") (Path.file "Dockerfile"))

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
      , CBT.publishPorts    = [CBT.PublishPort{container = port, host = empty}]
      , CBT.remove          = CBT.Remove
      , CBT.removeOnRunFail = CBT.Remove
      }

    port = CBT.Port 5432
