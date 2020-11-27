{-# LANGUAGE DeriveLift #-}

module CBT.Types where

import CBT.Prelude
import Control.Exception (Exception)
import Data.Hashable (Hashable)
import Data.Word (Word16)
import Instances.TH.Lift ()
import Language.Haskell.TH.Syntax (Lift)

import qualified System.Path as Path

data Detach         = Detach | Foreground
data Implementation = Docker | Podman
data Remove         = Remove | NoRemove

data Verbosity = Verbose | Quiet

data BuildSource
  = Directory Path.AbsRelDir
  | Instructions DockerfileContent

data BuildDefinition = BuildDefinition
  { source    :: BuildSource
  , imageName :: ImageName
  , verbosity :: Verbosity
  }

data Mount = Mount
  { containerPath :: Path.AbsDir
  , hostPath      :: Path.AbsDir
  }

data Command = Command
  { arguments :: [Text]
  , name      :: Text
  }

data ContainerDefinition = ContainerDefinition
  { command         :: Maybe Command
  , containerName   :: ContainerName
  , detach          :: Detach
  , imageName       :: ImageName
  , mounts          :: [Mount]
  , publishPorts    :: [PublishPort]
  , remove          :: Remove
  , removeOnRunFail :: Remove
  , workDir         :: Maybe Path.AbsDir
  }

mkCommand :: Text -> Command
mkCommand name = Command { arguments = [], .. }

minimalContainerDefinition :: ImageName -> ContainerName -> ContainerDefinition
minimalContainerDefinition imageName containerName =
  ContainerDefinition
  { command         = empty
  , detach          = Foreground
  , mounts          = []
  , publishPorts    = []
  , remove          = Remove
  , removeOnRunFail = Remove
  , workDir         = empty
  , ..
  }

newtype ContainerName = ContainerName Text
  deriving (Conversion Text) via Text

data PublishPort = PublishPort
  { container :: Port
  , host      :: Maybe Port
  }

newtype Port = Port { unPort :: Word16 }

instance Conversion Text Port where
  convert (Port port) = convertText $ show port

newtype DockerfileContent = DockerfileContent Text
  deriving (Conversion Text) via Text
  deriving stock Lift

newtype Prefix = Prefix Text
  deriving (Conversion Text) via Text

newtype Username = Username Text
  deriving (Conversion Text) via Text

newtype Password = Password Text
  deriving (Conversion Text) via Text

newtype Destination = Destination Text
  deriving (Conversion Text) via Text

newtype Registry = Registry Text
  deriving (Conversion Text) via Text

newtype ImageName = ImageName Text
  deriving (Conversion Text) via Text
  deriving newtype (Hashable)
  deriving stock   (Eq, Show)

data Status = Running | Absent
  deriving stock Show

instance Conversion Text Status where
  convert = convertText . show

newtype ImageBuildError = ImageBuildError Text
  deriving stock Show

instance Exception ImageBuildError
