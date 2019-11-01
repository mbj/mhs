module DBT.Podman
  ( Detach(..)
  , ImageName(..)
  , getHostPort
  , getImage
  , getMasterPassword
  , localConfig
  , run
  , start
  , status
  , stop
  , withPostgresqlEnv
  )
where

import Control.Monad (unless)
import DBT.Image
import DBT.Prelude
import DBT.Process
import Data.Monoid (mconcat)
import System.Path ((</>))

import qualified DBT.Build             as Build
import qualified DBT.Path              as Path
import qualified DBT.Postgresql        as Postgresql
import qualified Data.List             as List
import qualified System.Environment    as Environment
import qualified System.Exit           as Exit
import qualified System.Path           as Path
import qualified System.Path.Directory as Path
import qualified System.Path.IO        as Path
import qualified System.Process.Typed  as Process

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
      , convertText Build.masterUsername
      , "postgres"
      , "-D", Path.toString Path.pgData
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

  unless exists Build.buildImage

  pure imageName

getMasterPassword :: MonadIO m => m Postgresql.Password
getMasterPassword =
  Postgresql.Password <$> captureText
    (postgresProc ["cat", Path.toString Path.pgMasterPasswordAbs])

getHostPort :: MonadIO m => m Postgresql.Port
getHostPort = Postgresql.Port <$> captureText proc
  where
    proc = Process.proc "podman"
      [ "inspect"
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
    mkField name exp = exp <> ('.':name)

    mkIndex :: String -> String -> String
    mkIndex index exp = mconcat ["(", "index", " ", exp, " ", index, ")"]

getPostgresqlEnv :: MonadIO m => m Postgresql.Env
getPostgresqlEnv = do
  hostPort       <- getHostPort
  masterPassword <- getMasterPassword

  pure Postgresql.Env
    { hostname       = localhost
    , masterUsername = Build.masterUsername
    , ..
    }

withPostgresqlEnv :: MonadIO m => Proc -> m ()
withPostgresqlEnv proc = do
  environment   <- liftIO Environment.getEnvironment
  postgresqlEnv <- Postgresql.mkEnv <$> getPostgresqlEnv

  Process.runProcess_ $ Process.setEnv (environment <> postgresqlEnv) proc

localConfig :: MonadIO m => m ()
localConfig = do
  Postgresql.Env{..} <- getPostgresqlEnv
  home               <- liftIO Path.getHomeDirectory

  let pgpass = home </> Path.relFile ".pgpass"
  liftIO . Path.writeFile
    pgpass $
      List.intercalate
        ":"
        [ convertText localhost
        , convertText hostPort
        , convertText masterUsername
        , convertText masterPassword
        ]

containerName :: ContainerName
containerName = ContainerName "dbt"

postgresProc :: [String] -> Proc
postgresProc arguments
  = containerProc
  $ ["setuidgid", convertText Build.masterUsername] <> arguments

containerProc :: [String] -> Proc
containerProc arguments
  = Process.proc "podman"
  $ ["run", "--rm", "--", convertText imageName] <> arguments

localhost :: Postgresql.Hostname
localhost = Postgresql.Hostname "127.0.0.1"

containerPort :: Postgresql.Port
containerPort = Postgresql.Port "5432"

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
    , convertText imageName
    ]
  ]
  where
    detachFlag :: [String]
    detachFlag = case detach of
      Detach     -> ["--detach"]
      Foreground -> empty
