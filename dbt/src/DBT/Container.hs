module DBT.Container
  ( defaultBuildDefinition
  , populateDatabase
  , populateDatabaseBuildDefinition
  , populateDatabaseBuildDefinitionDefault
  , populateDatabaseBuildDefinitionIfAbsent
  , readClientConfig
  , withDatabaseContainer
  , withDatabaseContainerDefault
  , withDatabaseContainerImage
  , withDatabaseContainerImageModifyDefinition
  , withDatabaseContainerProcess
  , withDatabaseContainerProcessRun_
  )
where

import DBT.ClientConfig
import DBT.Prelude
import System.Path ((</>))

import qualified CBT
import qualified CBT.Container
import qualified CBT.Image
import qualified CBT.Image.BuildDefinition as CBT.Image
import qualified CBT.Image.Name            as CBT.Image
import qualified CBT.TH
import qualified DBT.Wait
import qualified Data.Text                 as Text
import qualified Data.Text.Encoding        as Text
import qualified MIO.Log                   as Log
import qualified System.Path               as Path
import qualified System.Process.Typed      as Process

withDatabaseContainerProcess
  :: CBT.Env env
  => CBT.Container.Prefix
  -> Process.ProcessConfig stdin stdout stderr
  -> (Process.ProcessConfig stdin stdout stderr -> (Process.Process stdin stdout stderr -> MIO env a) -> MIO env a)
  -> (Process.Process stdin stdout stderr -> MIO env a)
  -> MIO env a
withDatabaseContainerProcess prefix proc withProcess action = do
  containerName <- CBT.Container.nextName prefix
  withDatabaseContainer defaultBuildDefinition containerName $ \clientConfig -> do
    env <- getEnv clientConfig
    withProcess (Process.setEnv env proc) action

withDatabaseContainerProcessRun_
  :: CBT.Env env
  => CBT.Container.Prefix
  -> Process.ProcessConfig stdin stdout stderr
  -> MIO env ()
withDatabaseContainerProcessRun_ prefix proc =
  withDatabaseContainerProcess prefix proc Process.withProcessWait_ Process.checkExitCode

populateDatabaseBuildDefinitionIfAbsent
  :: (CBT.Env env, CBT.Image.IsName buildImageName, CBT.Image.IsName targetImageName)
  => CBT.Image.BuildDefinition buildImageName
  -> CBT.Container.Name
  -> targetImageName
  -> (ClientConfig -> MIO env ())
  -> MIO env ()
populateDatabaseBuildDefinitionIfAbsent buildDefinition containerName targetImageName action = do
  present <- CBT.Image.isPresent targetImageName
  if present
    then pure ()
    else populateDatabaseBuildDefinition buildDefinition containerName targetImageName action

populateDatabaseBuildDefinition
  :: (CBT.Env env, CBT.Image.IsName buildImageName, CBT.Image.IsName targetImageName)
  => CBT.Image.BuildDefinition buildImageName
  -> CBT.Container.Name
  -> targetImageName
  -> (ClientConfig -> MIO env ())
  -> MIO env ()
populateDatabaseBuildDefinition buildDefinition containerName targetImageName
  = CBT.Container.withBuildRun buildDefinition containerDefinition'
  . populate containerName targetImageName
  where
    containerDefinition' = (containerDefinition buildDefinition.imageName containerName)
      { CBT.Container.stopRemove = CBT.Container.StopNoRemove
      }

populateDatabase
  :: ( CBT.Env env
     , CBT.Image.IsName baseImageName
     , CBT.Image.IsName targetImageName
     )
  => baseImageName
  -> CBT.Container.Name
  -> targetImageName
  -> (ClientConfig -> MIO env ())
  -> MIO env ()
populateDatabase baseImageName containerName targetImageName
  = CBT.Container.withRun containerDefinition'
  . populate containerName targetImageName
  where
    containerDefinition' = (containerDefinition baseImageName containerName)
      { CBT.Container.stopRemove = CBT.Container.StopNoRemove
      , CBT.Container.pull       = pure CBT.Container.PullNever
      }

populate
  :: (CBT.Env env, CBT.Image.IsName targetImageName)
  => CBT.Container.Name
  -> targetImageName
  -> (ClientConfig -> MIO env ())
  -> MIO env ()
populate containerName targetImageName action = do
  runAction containerName action
  CBT.Container.stop containerName
  CBT.Container.commit containerName targetImageName

populateDatabaseBuildDefinitionDefault
  :: (CBT.Env env, CBT.Image.IsName imageName)
  => CBT.Container.Name
  -> imageName
  -> (ClientConfig -> MIO env ())
  -> MIO env ()
populateDatabaseBuildDefinitionDefault = populateDatabaseBuildDefinition defaultBuildDefinition

withDatabaseContainer
  :: (CBT.Env env, CBT.Image.IsName imageName)
  => CBT.Image.BuildDefinition imageName
  -> CBT.Container.Name
  -> (ClientConfig -> MIO env a)
  -> MIO env a
withDatabaseContainer buildDefinition containerName
  = CBT.Container.withBuildRun
      buildDefinition
      (containerDefinition buildDefinition.imageName containerName)
  . runAction containerName

withDatabaseContainerDefault
  :: forall env a . CBT.Env env
  => CBT.Container.Name
  -> (ClientConfig -> MIO env a)
  -> MIO env a
withDatabaseContainerDefault = withDatabaseContainer defaultBuildDefinition

withDatabaseContainerImage
  :: forall env imageName a . (CBT.Env env, CBT.Image.IsName imageName)
  => CBT.Container.Name
  -> imageName
  -> (ClientConfig -> MIO env a)
  -> MIO env a
withDatabaseContainerImage containerName targetImageName
  = withDatabaseContainerImageModifyDefinition containerName targetImageName identity

withDatabaseContainerImageModifyDefinition
  :: forall env imageName a . (CBT.Env env, CBT.Image.IsName imageName)
  => CBT.Container.Name
  -> imageName
  -> (CBT.Container.Definition imageName -> CBT.Container.Definition imageName)
  -> (ClientConfig -> MIO env a)
  -> MIO env a
withDatabaseContainerImageModifyDefinition containerName targetImageName modify
  = CBT.Container.withRun containerDefinition'
  . runAction containerName
  where
    containerDefinition' :: CBT.Container.Definition imageName
    containerDefinition'
      = modify
      $ setImageName
        (containerDefinition defaultBuildDefinition.imageName containerName)
        targetImageName

setImageName
  :: CBT.Image.IsName imageName
  => CBT.Container.Definition anyName
  -> imageName
  -> CBT.Container.Definition imageName
setImageName CBT.Container.Definition{..} imageName'
  = CBT.Container.Definition
  { CBT.Container.imageName = CBT.Image.toName imageName'
  , ..
  }

runAction
  :: forall env a . CBT.Env env
  => CBT.Container.Name
  -> (ClientConfig -> MIO env a)
  -> MIO env a
runAction containerName action = do
  Log.info $ "container name: " <> convert containerName

  clientConfig <- readClientConfig containerName

  waitForPort containerName clientConfig

  action clientConfig

readClientConfig
  :: CBT.Env env
  => CBT.Container.Name
  -> MIO env ClientConfig
readClientConfig containerName =
  mkClientConfig <$> getHostPort containerName <*> getMasterPassword containerName

getHostPort
  :: forall env . CBT.Env env
  => CBT.Container.Name
  -> MIO env HostPort
getHostPort containerName
  =   HostPort
  .   convert
  <$> CBT.Container.getHostPort containerName containerPort

getMasterPassword
  :: forall env . CBT.Env env
  => CBT.Container.Name
  -> MIO env Password
getMasterPassword containerName =
  Password . rstrip . Text.decodeUtf8 <$>
    CBT.Container.readFile containerName pgMasterPasswordAbs
  where
    rstrip = Text.dropWhileEnd (== '\n')

getClientConfig
  :: CBT.Env env
  => CBT.Container.Name
  -> MIO env ClientConfig
getClientConfig containerName =
  mkClientConfig
    <$> getHostPort containerName
    <*> getMasterPassword containerName

containerPort :: CBT.Container.Port
containerPort = CBT.Container.Port 5432

localhost :: HostName
localhost = HostName "127.0.0.1"

pgData :: Path.AbsDir
pgData = pgHome </> Path.relDir "data"

pgHome :: Path.AbsDir
pgHome = Path.absDir "/var/lib/postgresql"

pgMasterPasswordAbs :: Path.AbsFile
pgMasterPasswordAbs = pgHome </> Path.relFile "master-password.txt"

masterUserName :: UserName
masterUserName = UserName "postgres"

mkClientConfig
  :: HostPort
  -> Password
  -> ClientConfig
mkClientConfig hostPort password =
  ClientConfig
    { databaseName = DatabaseName "postgres"
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
  -> ClientConfig
  -> MIO env ()
waitForPort containerName clientConfig
  = DBT.Wait.wait
  $ DBT.Wait.Config
  { prefix      = "[DBT]"
  , maxAttempts = 100
  , waitTime    = 100000  -- 100ms
  , onFail      = CBT.Container.printInspect containerName >> CBT.Container.printLogs containerName
  , ..
  }

defaultBuildDefinition :: CBT.Image.BuildDefinition CBT.Image.TaggedName
defaultBuildDefinition
  =  CBT.Image.fromDockerfileContent (CBT.Image.mkLocalName "dbt")
  $$(CBT.TH.readDockerfileContent $ Path.file "Dockerfile")

postgresqlDefinition
  :: CBT.Image.IsName imageName
  => imageName
  -> CBT.Container.Name
  -> [Text]
  -> CBT.Container.Definition imageName
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
  :: CBT.Image.IsName imageName
  => imageName
  -> CBT.Container.Name
  -> CBT.Container.Definition imageName
containerDefinition imageName containerName
  = daemonize
  $ postgresqlDefinition
    imageName
    containerName
    [ "postgres"
    , "-D", convert $ Path.toString pgData
    , "-h", "0.0.0.0"  -- connections from outside the container
    , "-k", ""         -- no unix socket
    ]
  where
    daemonize :: CBT.Container.Definition a -> CBT.Container.Definition a
    daemonize value = value
      { CBT.Container.detach              = CBT.Container.Detach
      , CBT.Container.publishPorts        = [CBT.Container.PublishPort{container = port, host = empty}]
      , CBT.Container.stopRemove          = CBT.Container.StopRemove
      , CBT.Container.stopRemoveOnRunFail = CBT.Container.StopRemove
      }

    port = CBT.Container.Port 5432
