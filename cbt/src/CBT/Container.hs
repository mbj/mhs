{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}

module CBT.Container
  ( Definition(..)
  , Detach(..)
  , Entrypoint(..)
  , EnvVariable(..)
  , Exec(..)
  , Mount(..)
  , Name(..)
  , Port(..)
  , Prefix(..)
  , PublishPort(..)
  , Pull(..)
  , StopRemove(..)
  , buildRun
  , commit
  , execReadStdout
  , getHostPort
  , minimalDefinition
  , mkEntrypoint
  , nextName
  , printInspect
  , printLogs
  , readFile
  , remove
  , run
  , runReadStdout
  , status
  , stop
  , withBuildRun
  , withRun
  )
where

import CBT.Config
import CBT.Prelude
import CBT.Proc
import Data.Monoid (mconcat)
import Data.Word (Word16)
import Text.Read (readMaybe)
import UnliftIO.Exception (Exception)

import qualified CBT.Backend.Tar           as Tar
import qualified CBT.Image
import qualified CBT.Image.BuildDefinition as CBT.Image
import qualified CBT.Image.Name            as CBT.Image
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Lazy      as LBS
import qualified Data.List                 as List
import qualified Data.UUID.V4              as UUID
import qualified System.Exit               as System
import qualified System.Path               as Path
import qualified UnliftIO.Exception        as Exception

data PublishPort = PublishPort
  { container :: Port
  , host      :: Maybe Port
  }
  deriving stock Show

newtype Port = Port Word16
  deriving newtype (Conversion Word16)
  deriving stock (Eq, Show)

instance Conversion Text Port where
  convert (Port port) = convertText $ show port

data Status = Running | Absent
  deriving stock Show

instance Conversion Text Status where
  convert = convertText . show

data Detach = Detach | Foreground
  deriving stock Show

data StopRemove = StopRemove | StopNoRemove
  deriving stock Show

newtype Name = Name Text
  deriving newtype (Conversion Text)
  deriving stock (Eq, Show)

data Mount = Mount
  { containerPath :: Path.AbsDir
  , hostPath      :: Path.AbsDir
  }
  deriving stock Show

data Entrypoint = Entrypoint
  { arguments :: [Text]
  , name      :: Text
  }
  deriving stock Show

data EnvVariable = EnvInherit Text | EnvSet Text Text
  deriving stock Show

data Pull = PullAlways | PullMissing | PullNever
  deriving stock Show

data Definition imageName where
  Definition
    :: CBT.Image.IsName imageName
    => { command               :: Maybe Entrypoint
       , detach                :: Detach
       , env                   :: [EnvVariable]
       , extraBackendArguments :: [Text]
       , imageName             :: imageName
       , mounts                :: [Mount]
       , name                  :: Name
       , publishPorts          :: [PublishPort]
       , pull                  :: Maybe Pull
       , stopRemove            :: StopRemove
       , stopRemoveOnRunFail   :: StopRemove
       , workDir               :: Maybe Path.AbsDir
       }
    -> Definition name

deriving stock instance Show (Definition imageName)

newtype ContainerRunFailure = ContainerRunFailure Name

instance Exception ContainerRunFailure

instance Show ContainerRunFailure where
  show (ContainerRunFailure name)
    = "Failed to run container with name: "
    <> show name

mkEntrypoint :: Text -> Entrypoint
mkEntrypoint name = Entrypoint { arguments = [], .. }

minimalDefinition :: CBT.Image.IsName imageName => imageName -> Name -> Definition name
minimalDefinition imageName name
  = Definition
  { command               = empty
  , detach                = Foreground
  , env                   = []
  , extraBackendArguments = []
  , mounts                = []
  , publishPorts          = []
  , pull                  = empty
  , stopRemove            = StopRemove
  , stopRemoveOnRunFail   = StopRemove
  , workDir               = empty
  , ..
  }

printLogs :: Env env => Name -> MIO env ()
printLogs name = runProcess_ =<< backendProc ["container" , "logs" , convertText name]

commit :: (Env env, CBT.Image.IsName imageName) => Name -> imageName -> MIO env ()
commit name imageName
  = runProcess_
  =<< backendProc
  [ "container"
  , "commit"
  , convertText name
  , CBT.Image.nameString imageName
  ]

printInspect :: Env env => Name -> MIO env ()
printInspect name = runProcess_ =<< backendProc ["container" , "inspect" , convertText name]

handleFailure
  :: forall env imageName a . Env env
  => Definition imageName
  -> a
  -> System.ExitCode
  -> MIO env a
handleFailure Definition{..} value = \case
  System.ExitSuccess -> pure value
  _ -> do
    case (stopRemove, stopRemoveOnRunFail) of
      (StopNoRemove, StopRemove) -> remove name
      _                          -> pure ()
    Exception.throwIO $ ContainerRunFailure name

runProc
  :: Env env
  => Definition imageName
  -> MIO env Proc
runProc Definition{..} = detachSilence <$> backendProc backendArguments
  where
    backendArguments :: [String]
    backendArguments = mconcat
      [
        [ "run"
        , "--name",     convertText name
        , "--platform", "linux/amd64"
        ]
      , detachFlag detach
      , envOptions env
      , mountOptions
      , publishOptions
      , pullOptions
      , maybe [] workDirOptions workDir
      , removeFlag
      , convert <$> extraBackendArguments
      , [ "--"
        , CBT.Image.nameString imageName
        ]
      ] <> commandArguments
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
        removeFlag = case stopRemove of
          StopRemove   -> ["--rm"]
          StopNoRemove -> []

        pullOptions :: [String]
        pullOptions =
          maybe [] (("--pull":) . pure . mkOption) pull
          where
            mkOption :: Pull -> String
            mkOption = \case
              PullAlways  -> "always"
              PullMissing -> "missing"
              PullNever   -> "never"

        commandArguments :: [String]
        commandArguments =
          maybe [] (\arg -> convert <$> arg.name:arg.arguments) command

    detachSilence :: Proc -> Proc
    detachSilence =
      case detach of
        Detach -> silenceStdout
        _      -> identity

data Exec = Exec
  { command       :: Entrypoint
  , containerName :: Name
  , detach        :: Detach
  , env           :: [EnvVariable]
  , workDir       :: Maybe Path.AbsDir
  }

execProc
  :: Env env
  => Exec
  -> MIO env Proc
execProc Exec{..} = backendProc backendArguments
  where
    backendArguments :: [String]
    backendArguments = mconcat
      [ ["exec"]
      , detachFlag detach
      , envOptions env
      , maybe [] workDirOptions workDir
      , [convert $ convert @Text containerName, convert command.name]
      , convert <$> command.arguments
      ]

stop
  :: Env env
  => Name
  -> MIO env ()
stop name = runProcess_ . silenceStdout =<< backendProc ["stop", convertText name]

withRun
  :: Env env
  => Definition imageName
  -> MIO env a
  -> MIO env a
withRun containerDefinition@Definition{..} =
  Exception.bracket_ (run containerDefinition) (stop name)

remove
  :: Env env
  => Name
  -> MIO env ()
remove name
  = runProcess_
  . silenceStdout
  =<< backendProc
  [ "container"
  , "rm"
  , convertText name
  ]

run
  :: Env env
  => Definition imageName
  -> MIO env ()
run containerDefinition =
  handleFailure containerDefinition () =<< runProcess =<< runProc containerDefinition

runReadStdout
  :: Env env
  => Definition imageName
  -> MIO env BS.ByteString
runReadStdout containerDefinition = do
  (exitCode, output) <- readProcessStdout =<< runProc containerDefinition'
  handleFailure containerDefinition (convert output) exitCode
  where
    containerDefinition' =
      case containerDefinition of
        Definition{..} -> Definition{detach = Foreground, ..}

execReadStdout
  :: Env env
  => Exec
  -> MIO env LBS.ByteString
execReadStdout = readProcessStdout_ <=< execProc

readFile
  :: Env env
  => Name
  -> Path.AbsFile
  -> MIO env BS.ByteString
readFile name path = do
  tar <- readProcessStdout_ =<< proc
  maybe notFound (pure . LBS.toStrict) . Tar.findEntry tar $ Path.takeFileName path
  where
    notFound :: MIO env BS.ByteString
    notFound = liftIO $ fail "Tar from docker did not contain expected entry"

    proc
      = backendProc
      [ "container"
      , "cp"
      , convertText name <> ":" <> Path.toString path
      , "-"
      ]

status
  :: Env env
  => Name
  -> MIO env Status
status name
  = mapStatus <$> (runProcess =<< proc)
  where
    mapStatus = \case
      System.ExitSuccess -> Running
      _                  -> Absent

    proc = silenceStdout <$> backendProc ["container", "inspect", convertText name]

getHostPort :: Env env => Name -> Port -> MIO env Port
getHostPort name containerPort' =
  proc
    >>= captureText
    >>= parsePort
  where
    template =
      mkTemplate $
        mkField "HostPort" $
          mkIndex "0" $
            mkIndex (show $ (convertText containerPort' :: String) <> "/tcp") $
              mkField "Ports" $
                mkField "NetworkSettings" ""

    proc
      = backendProc
      [ "container"
      , "inspect"
      , convertText name
      , "--format"
      , template
      ]

    mkTemplate :: String -> String
    mkTemplate exp = mconcat ["{{", exp, "}}"]

    mkField :: String -> String -> String
    mkField key exp = exp <> ('.':key)

    mkIndex :: String -> String -> String
    mkIndex index exp = mconcat ["(", "index", " ", exp, " ", index, ")"]

    parsePort :: (ToText a, Show a) => a -> MIO env Port
    parsePort input = maybe failParse (pure . Port) . readMaybe $ convertText input
      where
        failParse :: MIO env Port
        failParse = Exception.throwString $ "Cannot parse port from input: " <> show input

newtype Prefix = Prefix Text
  deriving newtype (Conversion Text)
  deriving stock (Eq, Show)

withBuildRun
  :: Env env
  => CBT.Image.BuildDefinition imageName
  -> Definition imageName
  -> MIO env a
  -> MIO env a
withBuildRun buildDefinition definition =
  Exception.bracket_
    (buildRun buildDefinition definition)
    (CBT.Container.stop definition.name)

buildRun
  :: Env env
  => CBT.Image.BuildDefinition imageName
  -> Definition imageName
  -> MIO env ()
buildRun buildDefinition containerDefinition =
  CBT.Image.buildIfAbsent buildDefinition >> CBT.Container.run containerDefinition

nextName :: Prefix -> MIO env Name
nextName prefix = do
  uuid <- liftIO UUID.nextRandom
  pure . Name $ toText prefix <> "-" <> convertText (show uuid)

detachFlag :: Detach -> [String]
detachFlag = \case
  Detach     -> ["--detach"]
  Foreground -> []

envOptions :: [EnvVariable] -> [String]
envOptions env = mconcat $ ("--env" :) . pure . convert . option <$> env
  where
    option = \case
      EnvInherit envName -> envName
      EnvSet envName value -> envName <> "=" <> value

workDirOptions :: Path.AbsDir -> [String]
workDirOptions dir = ["--workdir", Path.toString dir]
