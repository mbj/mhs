{-# LANGUAGE InstanceSigs #-}

module CBT.Backend (Backend(..)) where

import CBT.Prelude
import CBT.Process
import CBT.Types
import Control.Exception (Exception)
import Control.Monad (unless)
import Data.Maybe (isJust)
import Data.Monoid (mconcat)
import System.Path ((</>))
import Text.Read (readMaybe)

import qualified CBT.Backend.Tar       as Tar
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.List             as List
import qualified Data.Text.Encoding    as Text
import qualified Data.Text.IO          as Text
import qualified System.Exit           as Exit
import qualified System.IO             as IO
import qualified System.Path           as Path
import qualified System.Path.Directory as Path
import qualified System.Process.Typed  as Process
import qualified UnliftIO.Exception    as Exception

newtype ContainerRunFailure = ContainerRunFailure
  { containerDefinition :: ContainerDefinition }

instance Exception ContainerRunFailure

instance Show ContainerRunFailure where
  show ContainerRunFailure{..} = "Failed to run container with name: " <> convertText (containerName containerDefinition)

class Backend (b :: Implementation) where
  binaryName        :: String
  getHostPort       :: MonadIO m => ContainerName -> Port -> m Port
  readContainerFile :: MonadIO m => ContainerName -> Path.AbsFile -> m BS.ByteString
  testImageExists   :: MonadIO m => BuildDefinition -> m Bool

  available :: MonadIO m => m Bool
  available = isJust <$> liftIO (Path.findExecutable (binaryName @b))

  buildIfAbsent :: MonadIO m => BuildDefinition -> m ()
  buildIfAbsent buildDefinition@BuildDefinition{..} = do
    exists <- testImageExists @b buildDefinition

    unless exists (build @b buildDefinition)

  status :: MonadIO m => ContainerName -> m Status
  status containerName
    = mapStatus <$> runProcess proc
    where
      mapStatus = \case
        Exit.ExitSuccess -> Running
        _                -> Absent

      proc = silence $ backendProc @b ["container", "inspect", convertText containerName]

  build :: forall m . MonadIO m => BuildDefinition -> m ()
  build BuildDefinition{..}
    = runProcess_
    $ Process.setStdin (Process.byteStringInput . LBS.fromStrict . Text.encodeUtf8 $ toText content)
    $ Process.proc (binaryName @b) ["build", "--tag", convertText imageName, "-"]

  buildRun :: MonadIO m => BuildDefinition -> ContainerDefinition -> m ()
  buildRun buildDefinition containerDefinition =
    buildIfAbsent @b buildDefinition >> run @b containerDefinition

  run :: forall m . MonadIO m => ContainerDefinition -> m ()
  run containerDefinition@ContainerDefinition{..}
    = handleFailure =<< (runProcess . backendProc @b $ containerArguments containerDefinition)
    where
      handleFailure :: Exit.ExitCode -> m ()
      handleFailure = \case
        Exit.ExitSuccess -> pure ()
        _ -> do
          doRemoveOnRunFail
          Exception.throwIO $ ContainerRunFailure containerDefinition

      doRemoveOnRunFail :: m ()
      doRemoveOnRunFail =
        case (remove, removeOnRunFail) of
          (NoRemove, Remove) -> removeContainer @b containerName
          _                  -> pure ()

  removeContainer :: MonadIO m => ContainerName -> m ()
  removeContainer containerName = runProcess_ $ backendProc @b ["container", "rm", convertText containerName]

  stop :: MonadIO m => ContainerName -> m ()
  stop containerName
    = runProcess_
    $ backendProc @b ["stop", convertText containerName]

  withContainer
    :: MonadUnliftIO m
    => BuildDefinition
    -> ContainerDefinition
    -> m a
    -> m a
  withContainer buildDefinition containerDefinition@ContainerDefinition{..} =
    Exception.bracket_ (buildRun @b buildDefinition containerDefinition) (stop @b containerName)

backendProc :: forall b . Backend b => [String] -> Process.ProcessConfig () () ()
backendProc = Process.proc (binaryName @b)

containerArguments :: ContainerDefinition -> [String]
containerArguments ContainerDefinition{..} = mconcat
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

    mkPublish :: Port -> [String]
    mkPublish (Port port) = ["--publish", "127.0.0.1::" <> show port]

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

  readContainerFile :: forall m . MonadIO m => ContainerName -> Path.AbsFile -> m BS.ByteString
  readContainerFile containerName path = do
    mountPath <- readMountPath
    liftIO . BS.readFile . Path.toString $ mountPath </> Path.makeRelative Path.rootDir path
    where
     readMountPath :: m Path.AbsDir
     readMountPath = Path.absDir . rstrip . convertText . Text.decodeUtf8 . LBS.toStrict <$> readProcess

     readProcess :: m LBS.ByteString
     readProcess
       = readProcessStdout_
       $ backendProc @'Podman
       [ "mount"
       , convertText containerName
       ]

     rstrip :: String -> String
     rstrip
       = List.reverse
       . List.dropWhile (== '\n')
       . List.reverse

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

  readContainerFile :: forall m . MonadIO m => ContainerName -> Path.AbsFile -> m BS.ByteString
  readContainerFile containerName path = do
    tar <- readProcessStdout_ proc
    maybe notFound (pure . LBS.toStrict) . Tar.findEntry tar $ Path.takeFileName path
    where
      notFound :: m BS.ByteString
      notFound = liftIO $ fail "Tar from docker did not contain expected entry"

      proc
        = Process.proc (binaryName @'Docker)
        [ "container"
        , "cp"
        , convertText containerName <> ":" <> Path.toString path
        , "-"
        ]


  testImageExists BuildDefinition{..} = exitBool <$> runProcess process
    where
      process
        = silence
        $ Process.proc (binaryName @'Docker)
        [ "inspect"
        , "--type", "image"
        , "--"
        , convertText imageName
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

parsePort :: forall m a . (MonadIO m, ToText a, Show a) => a -> m Port
parsePort input = maybe failParse (pure . Port) . readMaybe $ convertText input
  where
    failParse :: m Port
    failParse = liftIO . fail $ "Cannot parse PostgresqlPort from input: " <> show input

runProcess
  :: forall m stdin stdout stderr . MonadIO m
  => Process.ProcessConfig stdin stdout stderr
  -> m Exit.ExitCode
runProcess proc = runProc proc Process.runProcess

runProcess_
  :: forall m stdin stdout stderr . MonadIO m
  => Process.ProcessConfig stdin stdout stderr
  -> m ()
runProcess_ proc = runProc proc Process.runProcess_

readProcessStdout_
  :: forall m stdin stdout stderr . MonadIO m
  => Process.ProcessConfig stdin stdout stderr
  -> m LBS.ByteString
readProcessStdout_ proc = runProc proc Process.readProcessStdout_

runProc
  :: forall m a stdin stdout stderr . MonadIO m
  => Process.ProcessConfig stdin stdout stderr
  -> (Process.ProcessConfig stdin stdout stderr -> IO a)
  -> m a
runProc proc action = liftIO $ do
  Text.hPutStrLn IO.stderr . convertText $ show proc
  action proc
