module DBT.Postgresql.Container
  ( defaultBuildDefinition
  , populateDatabaseImage
  , populateDatabaseImageDefault
  , populateDatabaseImageIfAbsent
  , withDatabaseContainer
  , withDatabaseContainerDefault
  , withDatabaseContainerImage
  , withDatabaseContainerProcess
  , withDatabaseContainerProcessRun_
  )
where

import DBT.Postgresql.Prelude
import System.Path ((</>))

import qualified CBT
import qualified CBT.Container
import qualified CBT.Image
import qualified CBT.Image.BuildDefinition as CBT.Image
import qualified CBT.Image.Name            as CBT.Image
import qualified CBT.TH
import qualified DBT.Postgresql            as Postgresql
import qualified DBT.Postgresql.Wait       as Wait
import qualified Data.Text                 as Text
import qualified Data.Text.Encoding        as Text
import qualified System.Path               as Path
import qualified System.Process.Typed      as Process

withDatabaseContainerProcess
  :: CBT.Env env
  => CBT.Container.Prefix
  -> Process.ProcessConfig stdin stdout stderr
  -> (Process.ProcessConfig stdin stdout stderr -> (Process.Process stdin stdout stderr -> RIO env a) -> RIO env a)
  -> (Process.Process stdin stdout stderr -> RIO env a)
  -> RIO env a
withDatabaseContainerProcess prefix proc withProcess action = do
  containerName <- CBT.Container.nextName prefix
  withDatabaseContainer defaultBuildDefinition containerName $ \clientConfig -> do
    env <- Postgresql.getEnv clientConfig
    withProcess (Process.setEnv env proc) action

withDatabaseContainerProcessRun_
  :: CBT.Env env
  => CBT.Container.Prefix
  -> Process.ProcessConfig stdin stdout stderr
  -> RIO env ()
withDatabaseContainerProcessRun_ prefix proc =
  withDatabaseContainerProcess prefix proc Process.withProcessWait_ Process.checkExitCode

populateDatabaseImageIfAbsent
  :: forall env . CBT.Env env
  => CBT.Image.BuildDefinition
  -> CBT.Container.Name
  -> CBT.Image.TaggedName
  -> (Postgresql.ClientConfig -> RIO env ())
  -> RIO env ()
populateDatabaseImageIfAbsent buildDefinition containerName targetImageName action = do
  present <- CBT.Image.isPresent targetImageName
  if present
    then pure ()
    else populateDatabaseImage buildDefinition containerName targetImageName action

populateDatabaseImage
  :: forall env . CBT.Env env
  => CBT.Image.BuildDefinition
  -> CBT.Container.Name
  -> CBT.Image.TaggedName
  -> (Postgresql.ClientConfig -> RIO env ())
  -> RIO env ()
populateDatabaseImage buildDefinition containerName targetImageName action =
  CBT.Container.withBuildRun buildDefinition containerDefinition' $ do
    runAction containerName action
    CBT.Container.stop containerName
    CBT.Container.commit containerName targetImageName
  where
    containerDefinition' = (containerDefinition (getField @"imageName" buildDefinition) containerName)
      { CBT.Container.stopRemove = CBT.Container.StopNoRemove
      }

populateDatabaseImageDefault
  :: forall env . CBT.Env env
  => CBT.Container.Name
  -> CBT.Image.TaggedName
  -> (Postgresql.ClientConfig -> RIO env ())
  -> RIO env ()
populateDatabaseImageDefault = populateDatabaseImage defaultBuildDefinition

withDatabaseContainer
  :: forall env a . CBT.Env env
  => CBT.Image.BuildDefinition
  -> CBT.Container.Name
  -> (Postgresql.ClientConfig -> RIO env a)
  -> RIO env a
withDatabaseContainer buildDefinition containerName
  = CBT.Container.withBuildRun
      buildDefinition
      (containerDefinition (getField @"imageName" buildDefinition) containerName)
  . runAction containerName

withDatabaseContainerDefault
  :: forall env a . CBT.Env env
  => CBT.Container.Name
  -> (Postgresql.ClientConfig -> RIO env a)
  -> RIO env a
withDatabaseContainerDefault = withDatabaseContainer defaultBuildDefinition

withDatabaseContainerImage
  :: forall env a . CBT.Env env
  => CBT.Container.Name
  -> CBT.Image.TaggedName
  -> (Postgresql.ClientConfig -> RIO env a)
  -> RIO env a
withDatabaseContainerImage containerName targetImageName
  = CBT.Container.withRun containerDefinition'
  . runAction containerName
  where
    containerDefinition' :: CBT.Container.Definition
    containerDefinition' = setImageName
      (containerDefinition (getField @"imageName" defaultBuildDefinition) containerName) targetImageName

setImageName :: CBT.Container.Definition -> CBT.Image.TaggedName -> CBT.Container.Definition
setImageName CBT.Container.Definition{..} imageName'
  = CBT.Container.Definition
  { CBT.Container.imageName = imageName'
  , ..
  }

runAction
  :: forall env a . CBT.Env env
  => CBT.Container.Name
  -> (Postgresql.ClientConfig -> RIO env a)
  -> RIO env a
runAction containerName action = do
  hostPort <- getHostPort       containerName
  password <- getMasterPassword containerName

  let config = mkClientConfig hostPort password

  waitForPort containerName config

  action config

getHostPort
  :: forall env . CBT.Env env
  => CBT.Container.Name
  -> RIO env Postgresql.HostPort
getHostPort containerName
  =   Postgresql.HostPort
  .   convert
  <$> CBT.Container.getHostPort containerName containerPort

getMasterPassword
  :: forall env . CBT.Env env
  => CBT.Container.Name
  -> RIO env Postgresql.Password
getMasterPassword containerName =
  Postgresql.Password . rstrip . Text.decodeUtf8 <$>
    CBT.Container.readFile containerName pgMasterPasswordAbs
  where
    rstrip = Text.dropWhileEnd (== '\n')

getClientConfig
  :: CBT.Env env
  => CBT.Container.Name
  -> RIO env Postgresql.ClientConfig
getClientConfig containerName =
  mkClientConfig
    <$> getHostPort containerName
    <*> getMasterPassword containerName

containerPort :: CBT.Container.Port
containerPort = CBT.Container.Port 5432

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
  :: forall env . CBT.Env env
  => CBT.Container.Name
  -> Postgresql.ClientConfig
  -> RIO env ()
waitForPort containerName clientConfig
  = Wait.wait
  $ Wait.Config
  { prefix      = "[DBT]"
  , maxAttempts = 100
  , waitTime    = 100000  -- 100ms
  , onFail      = CBT.Container.printInspect containerName >> CBT.Container.printLogs containerName
  , ..
  }

defaultBuildDefinition :: CBT.Image.BuildDefinition
defaultBuildDefinition
  =  CBT.Image.fromDockerfileContent (CBT.Image.mkLocalName "dbt")
  $$(CBT.TH.readDockerfileContent $ Path.file "Dockerfile")

postgresqlDefinition
  :: CBT.Image.TaggedName
  -> CBT.Container.Name
  -> [Text]
  -> CBT.Container.Definition
postgresqlDefinition imageName containerName arguments =
  (CBT.Container.minimalDefinition imageName containerName)
    { CBT.Container.command      = pure command
    , CBT.Container.mounts       = []
    , CBT.Container.publishPorts = []
    , CBT.Container.workDir      = pure pgHome
    }
  where
    command = CBT.Container.Entrypoint
      { CBT.Container.name      = "setuidgid"
      , CBT.Container.arguments = convert masterUserName : arguments
      }

containerDefinition
  :: CBT.Image.TaggedName
  -> CBT.Container.Name
  -> CBT.Container.Definition
containerDefinition imageName containerName
  = deamonize
  $ postgresqlDefinition
    imageName
    containerName
    [ "postgres"
    , "-D", convert $ Path.toString pgData
    , "-h", "0.0.0.0"  -- connections from outside the container
    , "-k", ""         -- no unix socket
    ]
  where
    deamonize value = value
      { CBT.Container.detach              = CBT.Container.Detach
      , CBT.Container.publishPorts        = [CBT.Container.PublishPort{container = port, host = empty}]
      , CBT.Container.stopRemove          = CBT.Container.StopRemove
      , CBT.Container.stopRemoveOnRunFail = CBT.Container.StopRemove
      }

    port = CBT.Container.Port 5432
