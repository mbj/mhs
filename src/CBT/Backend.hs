module CBT.Backend
  ( Backend(..)
  , available
  , build
  , buildIfAbsent
  , buildRun
  , commit
  , login
  , printInspect
  , printLogs
  , push
  , readContainerFile
  , removeContainer
  , runReadStdout
  , status
  , stop
  , withContainer
  , withContainerDefinition
  )
where

import CBT.Environment
import CBT.Prelude
import CBT.Types
import Control.Exception (Exception)
import Control.Monad (unless)
import Data.Maybe (isJust)
import Data.Monoid (mconcat)
import Text.Read (readMaybe)

import qualified CBT.Backend.Tar       as Tar
import qualified CBT.IncrementalState  as IncrementalState
import qualified Colog
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.List             as List
import qualified Data.Text             as Text
import qualified Data.Text.Encoding    as Text
import qualified System.Exit           as Exit
import qualified System.Path           as Path
import qualified System.Path.Directory as Path
import qualified System.Process.Typed  as Process
import qualified UnliftIO.Exception    as Exception

newtype ContainerRunFailure = ContainerRunFailure
  { containerDefinition :: ContainerDefinition }

instance Exception ContainerRunFailure

instance Show ContainerRunFailure where
  show ContainerRunFailure{..}
    = "Failed to run container with name: "
    <> convertText (containerName containerDefinition)

class Backend (b :: Implementation) where
  binaryName        :: String
  getHostPort       :: WithEnv env => ContainerName -> Port -> RIO env Port
  testImageExists   :: WithEnv env => BuildDefinition -> RIO env Bool


instance Backend 'Podman where
  binaryName = "podman"

  getHostPort containerName containerPort' = parsePort =<< captureText proc
    where
      proc = Process.proc (binaryName @'Podman)
        [ "container"
        , "inspect"
        , convertText containerName
        , "--format"
        , template
        ]

      template =
        mkTemplate $
          mkField "HostPort" $
            mkIndex "0" $
              mkIndex (show $ (convertText containerPort' :: String) <> "/tcp") $
                mkField "PortBindings" $
                  mkField "HostConfig" ""

  testImageExists BuildDefinition{..} = exitBool <$> runProcess process
    where
      process =
        Process.proc
          (binaryName @'Podman)
          [ "image"
          , "exists"
          , "--"
          , convertText imageName
          ]

instance Backend 'Docker where
  binaryName = "docker"

  getHostPort containerName containerPort' = parsePort =<< captureText proc
    where
      proc = Process.proc (binaryName @'Docker)
        [ "container"
        , "inspect"
        , convertText containerName
        , "--format"
        , template
        ]

      template =
        mkTemplate $
          mkField "HostPort" $
            mkIndex "0" $
              mkIndex (show $ (convertText containerPort' :: String) <> "/tcp") $
                mkField "Ports" $
                  mkField "NetworkSettings" ""

  testImageExists BuildDefinition{..} = exitBool <$> runProcess process
    where
      process
        = silenceStdout
        $ Process.proc (binaryName @'Docker)
        [ "inspect"
        , "--type", "image"
        , "--"
        , convertText imageName
        ]

available :: forall b env . Backend b => RIO env Bool
available = isJust <$> liftIO (Path.findExecutable (binaryName @b))

buildIfAbsent
  :: forall b env . (Backend b, WithEnv env)
  => BuildDefinition
  -> RIO env ()
buildIfAbsent buildDefinition@BuildDefinition{..} = do
  exists <- testImageExists @b buildDefinition

  unless exists (build @b buildDefinition)

printLogs
  :: forall b env . (Backend b, WithEnv env)
  => ContainerName
  -> RIO env ()
printLogs containerName
  = runProcess_
  $ backendProc @b
  [ "container"
  , "logs"
  , convertText containerName
  ]

commit
  :: forall b env . (Backend b, WithEnv env)
  => ContainerName
  -> ImageName
  -> RIO env ()
commit containerName imageName
  = runProcess_
  $ backendProc @b
  [ "container"
  , "commit"
  , convertText containerName
  , convertText imageName
  ]

printInspect
  :: forall b env . (Backend b, WithEnv env)
  => ContainerName
  -> RIO env ()
printInspect containerName
  = runProcess_
  $ backendProc @b
  [ "container"
  , "inspect"
  , convertText containerName
  ]

status
  :: forall b env . (Backend b, WithEnv env)
  => ContainerName
  -> RIO env Status
status containerName
  = mapStatus <$> runProcess proc
  where
    mapStatus = \case
      Exit.ExitSuccess -> Running
      _                -> Absent

    proc = silenceStdout $ backendProc @b ["container", "inspect", convertText containerName]

build
  :: forall b env . (Backend b, WithEnv env)
  => BuildDefinition
  -> RIO env ()
build BuildDefinition{..}
  = do
    Environment{..} <- getEnvironment <$> ask
    IncrementalState.runBuildThrow builds imageName
      . fmap fromExit
      . runProcess
      $ setVerbosity verbosity proc
  where
    fromExit :: Exit.ExitCode -> Either ImageBuildError ()
    fromExit = \case
      Exit.ExitSuccess -> pure ()
      _                -> Left $ ImageBuildError "process exited with nonzero"

    proc = case source of
      Directory path       -> buildProc [Path.toString path]
      Instructions content -> fromInstructions content

    fromInstructions content
      = Process.setStdin
        ( Process.byteStringInput
        . LBS.fromStrict
        . Text.encodeUtf8
        $ toText content
        )
      $ buildProc ["-"]

    buildProc arguments
      = Process.proc (binaryName @b)
      $ ["build", "--tag", convertText imageName] <> arguments

buildRun
  :: forall b env . (Backend b, WithEnv env)
  => BuildDefinition
  -> ContainerDefinition -> RIO env ()
buildRun buildDefinition containerDefinition =
  buildIfAbsent @b buildDefinition >> run @b containerDefinition

run
  :: forall b env . (Backend b, WithEnv env)
  => ContainerDefinition
  -> RIO env ()
run containerDefinition@ContainerDefinition{..} =
  handleFailure @b containerDefinition () =<< runProcess (runProc @b containerDefinition)

runReadStdout
  :: forall b env . (Backend b, WithEnv env)
  => ContainerDefinition
  -> RIO env BS.ByteString
runReadStdout containerDefinition = do
  (exitCode, output) <- readProcessStdout $ runProc @b containerDefinition'
  handleFailure @b containerDefinition (convert output) exitCode
  where
    containerDefinition' = containerDefinition { detach = Foreground }

readContainerFile
  :: forall b env . (Backend b, WithEnv env)
  => ContainerName
  -> Path.AbsFile
  -> RIO env BS.ByteString
readContainerFile containerName path = do
  tar <- readProcessStdout_ proc
  maybe notFound (pure . LBS.toStrict) . Tar.findEntry tar $ Path.takeFileName path
  where
    notFound :: RIO env BS.ByteString
    notFound = liftIO $ fail "Tar from docker did not contain expected entry"

    proc
      = Process.proc (binaryName @b)
      [ "container"
      , "cp"
      , convertText containerName <> ":" <> Path.toString path
      , "-"
      ]

removeContainer
  :: forall b env . (Backend b, WithEnv env)
  => ContainerName
  -> RIO env ()
removeContainer containerName
  = runProcess_
  . silenceStdout
  $ backendProc @b
  [ "container"
  , "rm"
  , convertText containerName
  ]

push
  :: forall b env . (Backend b, WithEnv env)
  => ImageName
  -> Destination
  -> RIO env ()
push imageName destination
  = runProcess_
  $ backendProc @b
  [ "push"
  , convertText imageName
  , convertText destination
  ]

login
  :: forall b env . (Backend b, WithEnv env)
  => Registry
  -> Username
  -> Password
  -> RIO env ()
login registry username password
  = runProcess_
  . Process.setStdin
    ( Process.byteStringInput
    . convert
    . Text.encodeUtf8
    $ toText password
    )
  $ backendProc @b
  [ "login"
  , "--username"
  , convertText username
  , "--password-stdin"
  , convertText registry
  ]

stop
  :: forall b env . (Backend b, WithEnv env)
  => ContainerName
  -> RIO env ()
stop containerName
  = runProcess_
  . silenceStdout
  $ backendProc @b ["stop", convertText containerName]

withContainer
  :: forall b env a . (Backend b, WithEnv env)
  => BuildDefinition
  -> ContainerDefinition
  -> RIO env a
  -> RIO env a
withContainer buildDefinition containerDefinition@ContainerDefinition{..} =
  Exception.bracket_
    (buildRun @b buildDefinition containerDefinition)
    (stop @b containerName)

withContainerDefinition
  :: forall b env a . (Backend b, WithEnv env)
  => ContainerDefinition
  -> RIO env a
  -> RIO env a
withContainerDefinition containerDefinition@ContainerDefinition{..} =
  Exception.bracket_ (run @b containerDefinition) (stop @b containerName)

backendProc :: forall b . Backend b => [String] -> Proc
backendProc = Process.proc (binaryName @b)

runProc
  :: forall b . Backend b
  => ContainerDefinition
  -> Proc
runProc ContainerDefinition{..} = detachSilence $ backendProc @b containerArguments
  where
    containerArguments :: [String]
    containerArguments = mconcat
      [
        [ "run"
        , "--name", convertText containerName
        , "--workdir", Path.toString workDir
        ]
      , detachFlag
      , mountOptions
      , publishOptions
      , removeFlag
      , [ "--"
        , convertText imageName
        ]
      ] <> [programName] <> programArguments
      where
        publishOptions :: [String]
        publishOptions = mconcat $ mkPublish <$> publishPorts

        mkPublish :: PublishPort -> [String]
        mkPublish PublishPort{..} = ["--publish", "127.0.0.1:" <> hostPort <> ":" <> containerPort]
          where
            hostPort      = maybe "" toPort host
            containerPort = toPort container

            toPort (Port port) = show port

        mountOptions :: [String]
        mountOptions = mconcat $ mkMount <$> mounts

        mkMount :: Mount -> [String]
        mkMount Mount{..} = ["--mount", bindMount]
          where
            bindMount
              = List.intercalate
              ","
              [ "type=bind"
              , "source="      <> Path.toString hostPath
              , "destination=" <> Path.toString containerPath
              ]

        removeFlag :: [String]
        removeFlag = case remove of
          Remove   -> ["--rm"]
          NoRemove -> []

        detachFlag :: [String]
        detachFlag = case detach of
          Detach     -> ["--detach"]
          Foreground -> []

    detachSilence :: Proc -> Proc
    detachSilence =
      case detach of
        Detach -> silenceStdout
        _      -> identity

type Proc = Process.ProcessConfig () () ()

setVerbosity :: Verbosity -> Proc -> Proc
setVerbosity = \case
  Quiet -> silence
  _     -> identity

silenceStderr :: Proc -> Proc
silenceStderr = Process.setStderr Process.nullStream

silenceStdout :: Proc -> Proc
silenceStdout = Process.setStdout Process.nullStream

silence :: Proc -> Proc
silence = silenceStdout . silenceStderr

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

parsePort :: forall env a . (ToText a, Show a) => a -> RIO env Port
parsePort input = maybe failParse (pure . Port) . readMaybe $ convertText input
  where
    failParse :: RIO env Port
    failParse = liftIO . fail $ "Cannot parse PostgresqlPort from input: " <> show input

runProcess
  :: forall env stdin stdout stderr . WithEnv env
  => Process.ProcessConfig stdin stdout stderr
  -> RIO env Exit.ExitCode
runProcess proc = procRun proc Process.runProcess

runProcess_
  :: forall env stdin stdout stderr . WithEnv env
  => Process.ProcessConfig stdin stdout stderr
  -> RIO env ()
runProcess_ proc = procRun proc Process.runProcess_

readProcessStdout_
  :: forall env stdin stdout stderr . WithEnv env
  => Process.ProcessConfig stdin stdout stderr
  -> RIO env LBS.ByteString
readProcessStdout_ proc = procRun proc Process.readProcessStdout_

readProcessStdout
  :: forall env stdin stdout stderr . WithEnv env
  => Process.ProcessConfig stdin stdout stderr
  -> RIO env (Exit.ExitCode, LBS.ByteString)
readProcessStdout proc = procRun proc Process.readProcessStdout

procRun
  :: forall a env stdin stdout stderr . WithEnv env
  => Process.ProcessConfig stdin stdout stderr
  -> (Process.ProcessConfig stdin stdout stderr -> IO a)
  -> RIO env a
procRun proc action = onDebug (Colog.logDebug . convert $ show proc) >> liftIO (action proc)

handleFailure
  :: forall b env a . (Backend b, WithEnv env)
  => ContainerDefinition
  -> a
  -> Exit.ExitCode
  -> RIO env a
handleFailure containerDefinition@ContainerDefinition{..} value = \case
  Exit.ExitSuccess -> pure value
  _ -> do
    case (remove, removeOnRunFail) of
      (NoRemove, Remove) -> removeContainer @b containerName
      _                  -> pure ()
    Exception.throwIO $ ContainerRunFailure containerDefinition

captureText
  :: forall env stdin stdout stderr . WithEnv env
  => Process.ProcessConfig stdin stdout stderr
  -> RIO env Text
captureText proc
  =   Text.strip
  .   Text.decodeUtf8
  .   LBS.toStrict
  <$> readProcessStdout_ proc
