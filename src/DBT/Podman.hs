module DBT.Podman
  ( Detach(..)
  , ImageName(..)
  , getClientConfig
  , getHostPort
  , getImage
  , getMasterPassword
  , localConfig
  , run
  , start
  , status
  , stop
  , withDatabaseContainer
  , withPostgresqlEnv
  )
where

import Control.Monad (unless)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import DBT.Image
import DBT.Prelude
import DBT.Process
import Data.Monoid (mconcat)
import System.Path ((</>))

import qualified DBT.Image             as Image
import qualified DBT.Postgresql        as Postgresql
import qualified Data.List             as List
import qualified System.Environment    as Environment
import qualified System.Exit           as Exit
import qualified System.Path           as Path
import qualified System.Path.Directory as Path
import qualified System.Path.IO        as Path
import qualified System.Process.Typed  as Process
import qualified UnliftIO.Exception    as Exception

newtype ContainerName = ContainerName Text
  deriving newtype ToText

data Status = Running | Absent
  deriving stock Show

instance ToText Status where
  toText = convertText . show

data Detach = Detach | Foreground

run :: MonadIO m => [String] -> m ()
run arguments =
  Process.runProcess_ . Process.proc "podman" $
    podmanArguments Foreground <> arguments

start :: MonadIO m => Detach -> [String] -> m ()
start detach arguments = do
  void getImage
  Process.runProcess_ $
    Process.proc "podman" $ podmanArguments detach <>
      [ "setuidgid"
      , convertText masterUserName
      , "postgres"
      , "-D", Path.toString pgData
      , "-h", "0.0.0.0"  -- connections from outside the container
      , "-k", ""         -- no unix socket
      ] <> arguments

stop :: MonadIO m => m ()
stop
  = Process.runProcess_
  $ Process.proc "podman" ["stop", convertText containerName]

status :: MonadIO m => m Status
status
  = mapStatus <$> Process.runProcess proc
  where
    mapStatus = \case
      Exit.ExitSuccess -> Running
      _                -> Absent

    proc = silence $ Process.proc
      "podman"
      ["container", "inspect", convertText containerName]

    silence :: Proc -> Proc
    silence
      = Process.setStderr Process.nullStream
      . Process.setStdout Process.nullStream

getImage :: MonadIO m => m ImageName
getImage = do
  exists <- testImageExists

  unless exists Image.build

  pure Image.name

getMasterPassword :: MonadIO m => m Postgresql.Password
getMasterPassword =
  Postgresql.Password <$> captureText
    (postgresProc ["cat", Path.toString pgMasterPasswordAbs])

getHostPort :: forall m . MonadIO m => m Postgresql.HostPort
getHostPort = Postgresql.parseHostPort =<< captureText proc
  where
    proc = Process.proc "podman"
      [ "container"
      , "inspect"
      , "dbt"
      , "--format"
      , template
      ]

    template =
      mkTemplate $
        mkField "HostPort" $
          mkIndex "0" $
            mkIndex (show $ (convertText containerPort :: String) <> "/tcp") $
              mkField "PortBindings" $
                mkField "HostConfig" ""

    mkTemplate :: String -> String
    mkTemplate exp = mconcat ["{{", exp, "}}"]

    mkField :: String -> String -> String
    mkField key exp = exp <> ('.':key)

    mkIndex :: String -> String -> String
    mkIndex index exp = mconcat ["(", "index", " ", exp, " ", index, ")"]

getClientConfig :: MonadIO m => m Postgresql.ClientConfig
getClientConfig = do
  hostPort <- pure <$> getHostPort
  password <- pure <$> getMasterPassword

  pure Postgresql.ClientConfig
    { databaseName = Postgresql.DatabaseName "postgres"
    , hostName     = localhost
    , sslMode      = empty
    , sslRootCert  = empty
    , userName     = masterUserName
    , ..
    }

withPostgresqlEnv :: MonadIO m => Proc -> m ()
withPostgresqlEnv proc = do
  environment   <- liftIO Environment.getEnvironment
  postgresqlEnv <- Postgresql.toEnv <$> getClientConfig

  Process.runProcess_ $ Process.setEnv (environment <> postgresqlEnv) proc

localConfig :: MonadIO m => m ()
localConfig = do
  Postgresql.ClientConfig{..} <- getClientConfig
  home                        <- liftIO Path.getHomeDirectory
  port                        <- maybe (liftIO $ fail "Local config without port") pure hostPort

  let pgpass = home </> Path.relFile ".pgpass"
  liftIO . Path.writeFile
    pgpass $
      List.intercalate
        ":"
        [ convertText localhost
        , convertText port
        , convertText userName
        , maybe "" convertText password
        ]

containerName :: ContainerName
containerName = ContainerName "dbt"

postgresProc :: [String] -> Proc
postgresProc arguments
  = containerProc
  $ ["setuidgid", convertText masterUserName] <> arguments

containerProc :: [String] -> Proc
containerProc arguments
  = Process.proc "podman"
  $ ["run", "--rm", "--", convertText Image.name] <> arguments

localhost :: Postgresql.HostName
localhost = Postgresql.HostName "127.0.0.1"

containerPort :: Postgresql.HostPort
containerPort = Postgresql.HostPort 5432

podmanArguments :: Detach -> [String]
podmanArguments detach = mconcat
  [
    [ "run"
    , "--interactive"
    , "--name",    convertText containerName
    , "--publish", "127.0.0.1::" <> convertText containerPort
    , "--rm"
    , "--tty"
    ]
  , detachFlag
  , [ "--"
    , convertText Image.name
    ]
  ]
  where
    detachFlag :: [String]
    detachFlag = case detach of
      Detach     -> ["--detach"]
      Foreground -> empty

withDatabaseContainer :: MonadUnliftIO m => (Postgresql.ClientConfig -> m a) -> m a
withDatabaseContainer action = tryReUse =<< status
  where
    tryReUse = \case
      Running -> reUse
      Absent  -> startOnce

    startOnce = do
      log @Text "[DBT] Creating new one off container"
      Exception.bracket_ (start Detach []) stop runAction

    reUse = do
      log @Text "[DBT] Re using existing database container"
      runAction

    runAction = action =<< getClientConfig

pgData :: Path.AbsDir
pgData = pgHome </> Path.relDir "data"

pgHome :: Path.AbsDir
pgHome = Path.absDir "/var/lib/postgresql"

pgMasterPasswordAbs :: Path.AbsFile
pgMasterPasswordAbs = pgHome </> Path.relFile "master-password.txt"

masterUserName :: Postgresql.UserName
masterUserName = Postgresql.UserName "postgres"
