{-# LANGUAGE DeriveLift #-}

module CBT.Types where

import CBT.Prelude
import Data.Word (Word16)
import Instances.TH.Lift ()
import Language.Haskell.TH.Syntax (Lift)

import qualified System.Path as Path

data Detach         = Detach | Foreground
data Implementation = Docker | Podman
data Remove         = Remove | NoRemove

data BuildDefinition = BuildDefinition
  { content   :: DockerfileContent
  , imageName :: ImageName
  }
  deriving stock Lift

data Mount = Mount
  { containerPath :: Path.AbsDir
  , hostPath      :: Path.AbsDir
  }

data ContainerDefinition = ContainerDefinition
  { containerName    :: ContainerName
  , detach           :: Detach
  , imageName        :: ImageName
  , mounts           :: [Mount]
  , programArguments :: [String]
  , programName      :: String
  , publishPorts     :: [Port]
  , remove           :: Remove
  , removeOnRunFail  :: Remove
  , workDir          :: Path.AbsDir
  }

newtype ContainerName = ContainerName Text
  deriving newtype ToText

newtype Port = Port Word16

instance ToText Port where
  toText (Port port) = convertText $ show port

newtype DockerfileContent = DockerfileContent Text
  deriving newtype ToText
  deriving stock Lift

newtype Prefix = Prefix Text
  deriving newtype ToText
  deriving stock Lift

newtype ImageName = ImageName Text
  deriving newtype ToText
  deriving stock Lift

data Status = Running | Absent
  deriving stock Show

instance ToText Status where
  toText = convertText . show

