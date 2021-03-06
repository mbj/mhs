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

import Control.Arrow (left)
import DBT.Postgresql.Prelude
import System.Path ((</>))

import qualified CBT
import qualified CBT.TH
import qualified DBT.Postgresql       as Postgresql
import qualified DBT.Postgresql.Wait  as Wait
import qualified Data.Text            as Text
import qualified Data.Text.Encoding   as Text
import qualified System.Path          as Path
import qualified System.Process.Typed as Process
import qualified UnliftIO.Exception   as Exception

withDatabaseContainerProcess
  :: CBT.WithEnv env
  => CBT.Prefix
  -> Process.ProcessConfig stdin stdout stderr
  -> (Process.ProcessConfig stdin stdout stderr -> (Process.Process stdin stdout stderr -> RIO env a) -> RIO env a)
  -> (Process.Process stdin stdout stderr -> RIO env a)
  -> RIO env a
withDatabaseContainerProcess prefix proc withProcess action = do
  containerName <- CBT.nextContainerName prefix
  withDatabaseContainer defaultBuildDefinition containerName $ \clientConfig -> do
    env <- Postgresql.getEnv clientConfig
    withProcess (Process.setEnv env proc) action

withDatabaseContainerProcessRun_
  :: CBT.WithEnv env
  => CBT.Prefix
  -> Process.ProcessConfig stdin stdout stderr
  -> RIO env ()
withDatabaseContainerProcessRun_ prefix proc =
  withDatabaseContainerProcess prefix proc Process.withProcessWait_ Process.checkExitCode

populateDatabaseImageIfAbsent
  :: forall env . (CBT.WithEnv env)
  => CBT.BuildDefinition
  -> CBT.ContainerName
  -> CBT.ImageName
  -> (Postgresql.ClientConfig -> RIO env ())
  -> RIO env (Either CBT.ImageBuildError ())
populateDatabaseImageIfAbsent buildDefinition containerName targetImageName action = do
  present <- CBT.isImagePresent targetImageName
  if present
    then pure $ pure ()
    else populateDatabaseImage buildDefinition containerName targetImageName action

populateDatabaseImage
  :: forall env . (CBT.WithEnv env)
  => CBT.BuildDefinition
  -> CBT.ContainerName
  -> CBT.ImageName
  -> (Postgresql.ClientConfig -> RIO env ())
  -> RIO env (Either CBT.ImageBuildError ())
populateDatabaseImage buildDefinition containerName targetImageName action =
   fmap toError . Exception.tryAnyDeep $ do
     Exception.bracket_
       (CBT.buildRun buildDefinition containerDefinition')
       (CBT.removeContainer containerName)
       run
  where
    containerDefinition' = (containerDefinition (getField @"imageName" buildDefinition) containerName)
      { CBT.remove = CBT.NoRemove
      }

    toError :: Either Exception.SomeException a -> Either CBT.ImageBuildError a
    toError = left (CBT.ImageBuildError . convert . show)

    run :: RIO env ()
    run = do
      runAction containerName action
      CBT.stop containerName
      CBT.commit containerName targetImageName

populateDatabaseImageDefault
  :: forall env . (CBT.WithEnv env)
  => CBT.ContainerName
  -> CBT.ImageName
  -> (Postgresql.ClientConfig -> RIO env ())
  -> RIO env (Either CBT.ImageBuildError ())
populateDatabaseImageDefault = populateDatabaseImage defaultBuildDefinition

withDatabaseContainer
  :: forall env a . (CBT.WithEnv env)
  => CBT.BuildDefinition
  -> CBT.ContainerName
  -> (Postgresql.ClientConfig -> RIO env a)
  -> RIO env a
withDatabaseContainer buildDefinition containerName
  = CBT.withContainerBuildRun
      buildDefinition
      (containerDefinition (getField @"imageName" buildDefinition) containerName)
  . runAction containerName

withDatabaseContainerDefault
  :: forall env a . (CBT.WithEnv env)
  => CBT.ContainerName
  -> (Postgresql.ClientConfig -> RIO env a)
  -> RIO env a
withDatabaseContainerDefault = withDatabaseContainer defaultBuildDefinition

withDatabaseContainerImage
  :: forall env a . (CBT.WithEnv env)
  => CBT.ContainerName
  -> CBT.ImageName
  -> (Postgresql.ClientConfig -> RIO env a)
  -> RIO env a
withDatabaseContainerImage containerName targetImageName
  = CBT.withContainerRun containerDefinition'
  . runAction containerName
  where
    containerDefinition' :: CBT.ContainerDefinition
    containerDefinition' = setImageName
      (containerDefinition (getField @"imageName" defaultBuildDefinition) containerName) targetImageName

setImageName :: CBT.ContainerDefinition -> CBT.ImageName -> CBT.ContainerDefinition
setImageName CBT.ContainerDefinition{..} imageName'
  = CBT.ContainerDefinition
  { CBT.imageName = imageName'
  , ..
  }

runAction
  :: forall env a . (CBT.WithEnv env)
  => CBT.ContainerName
  -> (Postgresql.ClientConfig -> RIO env a)
  -> RIO env a
runAction containerName action = do
  hostPort <- getHostPort       containerName
  password <- getMasterPassword containerName

  let config = mkClientConfig hostPort password

  waitForPort containerName config

  action config

getHostPort
  :: forall env . (CBT.WithEnv env)
  => CBT.ContainerName
  -> RIO env Postgresql.HostPort
getHostPort containerName
  =   Postgresql.HostPort
  .   CBT.unPort
  <$> CBT.getHostPort containerName containerPort

getMasterPassword
  :: forall env . (CBT.WithEnv env)
  => CBT.ContainerName
  -> RIO env Postgresql.Password
getMasterPassword containerName =
  Postgresql.Password . rstrip . Text.decodeUtf8 <$>
    CBT.readContainerFile containerName pgMasterPasswordAbs
  where
    rstrip = Text.dropWhileEnd (== '\n')

getClientConfig
  :: CBT.WithEnv env
  => CBT.ContainerName
  -> RIO env Postgresql.ClientConfig
getClientConfig containerName =
  mkClientConfig
    <$> getHostPort containerName
    <*> getMasterPassword containerName

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
  :: forall env . (CBT.WithEnv env)
  => CBT.ContainerName
  -> Postgresql.ClientConfig
  -> RIO env ()
waitForPort containerName clientConfig
  = Wait.wait
  $ Wait.Config
  { prefix      = "[DBT]"
  , maxAttempts = 100
  , waitTime    = 100000  -- 100ms
  , onFail      = CBT.printInspect containerName >> CBT.printLogs containerName
  , ..
  }

defaultBuildDefinition :: CBT.BuildDefinition
defaultBuildDefinition
  =  CBT.fromDockerfileContent (CBT.Prefix "dbt")
  $$(CBT.TH.readDockerfileContent $ Path.file "Dockerfile")

postgresqlDefinition
  :: CBT.ImageName
  -> CBT.ContainerName
  -> [Text]
  -> CBT.ContainerDefinition
postgresqlDefinition imageName containerName arguments =
  (CBT.minimalContainerDefinition imageName containerName)
    { CBT.command      = pure command
    , CBT.mounts       = []
    , CBT.publishPorts = []
    , CBT.workDir      = pure pgHome
    }
  where
    command = CBT.Command
      { CBT.name      = "setuidgid"
      , CBT.arguments = convert masterUserName : arguments
      }

containerDefinition :: CBT.ImageName -> CBT.ContainerName -> CBT.ContainerDefinition
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
      { CBT.detach          = CBT.Detach
      , CBT.publishPorts    = [CBT.PublishPort{container = port, host = empty}]
      , CBT.remove          = CBT.Remove
      , CBT.removeOnRunFail = CBT.Remove
      }

    port = CBT.Port 5432
