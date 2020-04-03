{-# LANGUAGE AllowAmbiguousTypes #-}

module DBT.Backend where

import Control.Monad (unless)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import DBT.Prelude
import DBT.Process
import Data.Maybe (isJust)
import Data.Monoid (mconcat)
import System.Path ((</>))

import qualified DBT.Image             as Image
import qualified DBT.Postgresql        as Postgresql
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.List             as List
import qualified System.Environment    as Environment
import qualified System.Exit           as Exit
import qualified System.Path           as Path
import qualified System.Path.Directory as Path
import qualified System.Path.IO        as Path
import qualified System.Process.Typed  as Process
import qualified UnliftIO.Exception    as Exception

data Implementation = Docker | Podman
data Detach         = Detach | Foreground

newtype ContainerName = ContainerName Text
  deriving newtype ToText

data Status = Running | Absent
  deriving stock Show

instance ToText Status where
  toText = convertText . show

class Backend (b :: Implementation) where
  binaryName      :: String
  getHostPort     :: MonadIO m => m Postgresql.HostPort
  testImageExists :: MonadIO m => m Bool

  available :: MonadIO m => m Bool
  available = isJust <$> liftIO (Path.findExecutable (binaryName @b))

  getImage :: MonadIO m => m Image.Name
  getImage = do
    exists <- testImageExists @b

    unless exists (build @b)

    pure Image.name

  status :: MonadIO m => m Status
  status
    = mapStatus <$> Process.runProcess proc
    where
      mapStatus = \case
        Exit.ExitSuccess -> Running
        _                -> Absent

      proc = silence $ Process.proc
        (binaryName @b)
        ["container", "inspect", convertText containerName]

  build :: forall m . MonadIO m => m ()
  build
    = Process.runProcess_
    $ Process.setStdin (Process.byteStringInput $ LBS.fromStrict Image.dockerfileContents)
    $ Process.proc (binaryName @b) ["build", "--tag", convertText Image.name, "-"]

  run :: MonadIO m => [String] -> m ()
  run arguments =
    Process.runProcess_ . Process.proc (binaryName @b) $
      containerArguments Foreground <> arguments

  start :: MonadIO m => Detach -> [String] -> m ()
  start detach arguments = do
    void (getImage @b)
    Process.runProcess_ $
      Process.proc (binaryName @b) $ containerArguments detach <>
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
    $ Process.proc (binaryName @b) ["stop", convertText containerName]

  containerProc :: [String] -> Proc
  containerProc arguments
    = Process.proc (binaryName @b)
    $ ["run", "--rm", "--", convertText Image.name] <> arguments

  postgresProc :: [String] -> Proc
  postgresProc arguments
    = containerProc @b
    $ ["setuidgid", convertText masterUserName] <> arguments

  getMasterPassword :: MonadIO m => m Postgresql.Password
  getMasterPassword =
    Postgresql.Password <$> captureText
      (postgresProc @b ["cat", Path.toString pgMasterPasswordAbs])

  getClientConfig :: MonadIO m => m Postgresql.ClientConfig
  getClientConfig = do
    hostPort <- pure <$> getHostPort @b
    password <- pure <$> getMasterPassword @b

    pure Postgresql.ClientConfig
      { databaseName = Postgresql.DatabaseName "postgres"
      , hostName     = localhost
      , sslMode      = empty
      , sslRootCert  = empty
      , userName     = masterUserName
      , ..
      }

  withDatabaseContainer :: MonadUnliftIO m => (Postgresql.ClientConfig -> m a) -> m a
  withDatabaseContainer action = tryReUse =<< status @b
    where
      tryReUse = \case
        Running -> reUse
        Absent  -> startOnce

      startOnce = do
        log @Text "[DBT] Creating new one off container"
        Exception.bracket_ (start @b Detach []) (stop @b) runAction

      reUse = do
        log @Text "[DBT] Re using existing database container"
        runAction

      runAction = action =<< getClientConfig @b

  withPostgresqlEnv :: MonadIO m => Proc -> m ()
  withPostgresqlEnv proc = do
    environment   <- liftIO Environment.getEnvironment
    postgresqlEnv <- Postgresql.toEnv <$> getClientConfig @b

    Process.runProcess_ $ Process.setEnv (environment <> postgresqlEnv) proc

  localConfig :: MonadIO m => m ()
  localConfig = do
    Postgresql.ClientConfig{..} <- getClientConfig @b
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

containerArguments :: Detach -> [String]
containerArguments detach = mconcat
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

containerName :: ContainerName
containerName = ContainerName "dbt"

containerPort :: Postgresql.HostPort
containerPort = Postgresql.HostPort 5432

instance Backend 'Podman where
  binaryName = "podman"

  getHostPort = Postgresql.parseHostPort =<< captureText proc
    where
      proc = Process.proc (binaryName @'Podman)
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

  testImageExists = exitBool <$> Process.runProcess process
    where
      process =
        Process.proc
          (binaryName @'Podman)
          [ "image"
          , "exists"
          , "--"
          , convertText Image.name
          ]

instance Backend 'Docker where
  binaryName = "docker"

  getHostPort = Postgresql.parseHostPort =<< captureText proc
    where
      proc = Process.proc (binaryName @'Docker)
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
                mkField "Ports" $
                  mkField "NetworkSettings" ""

  testImageExists = exitBool <$> Process.runProcess process
    where
      process
        = silence
        $ Process.proc (binaryName @'Docker)
        [ "inspect"
        , "--type", "image"
        , "--"
        , convertText Image.name
        ]

silence :: Proc -> Proc
silence
  = Process.setStderr Process.nullStream
  . Process.setStdout Process.nullStream

mkTemplate :: String -> String
mkTemplate exp = mconcat ["{{", exp, "}}"]

mkField :: String -> String -> String
mkField key exp = exp <> ('.':key)

mkIndex :: String -> String -> String
mkIndex index exp = mconcat ["(", "index", " ", exp, " ", index, ")"]

exitBool :: Exit.ExitCode -> Bool
exitBool = \case
  Exit.ExitSuccess -> True
  _                -> False
