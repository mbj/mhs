module LHT.Build
  ( Config(..)
  , Flag(..)
  , PackageName(..)
  , TargetName(..)
  , build
  , buildZip
  )
where

import LHT.Prelude
import System.Path ((</>))

import qualified CBT
import qualified CBT.TH
import qualified Data.ByteString       as BS
import qualified Data.Foldable         as Foldable
import qualified LHT.Zip               as Zip
import qualified System.Path           as Path
import qualified System.Path.Directory as Path
import qualified System.Posix.Files    as Files
import qualified UnliftIO.Exception    as Exception

newtype PackageName = PackageName Text
  deriving (Conversion Text) via Text

newtype TargetName = TargetName Text
  deriving (Conversion Text) via Text

data Flag = Flag PackageName Text

data Config = Config
  { executablePath :: Path.RelFile
  , flags          :: [Flag]
  , packageName    :: PackageName
  , targetName     :: TargetName
  }

prefix :: CBT.Prefix
prefix = CBT.Prefix "lht"

buildDefinition :: CBT.BuildDefinition
buildDefinition
  =  CBT.fromDockerfileContent prefix
  $$(CBT.TH.readDockerfileContent $ Path.file "Dockerfile")

buildZip
  :: CBT.WithEnv env
  => Config
  -> RIO env BS.ByteString
buildZip config = fmap (convert . Zip.mkZip) (build config)

build
  :: CBT.WithEnv env
  => Config
  -> RIO env BS.ByteString
build config@Config{..} =
  withBuildContainer config $ \containerName ->
    CBT.readContainerFile containerName (containerHomePath </> executablePath)

withBuildContainer
  :: CBT.WithEnv env
  => Config
  -> (CBT.ContainerName -> RIO env a)
  -> RIO env a
withBuildContainer Config{..} action = do
  containerName <- CBT.nextContainerName prefix

  hostProjectPath <- liftIO Path.getCurrentDirectory
  hostHomePath    <- liftIO Path.getHomeDirectory

  let
    containerProjectPath :: Path.AbsDir
    containerProjectPath = containerHomePath </> Path.relDir (convertText packageName)

    containerStackPath :: Path.AbsDir
    containerStackPath = containerHomePath </> Path.relDir ".stack"

    hostStackPath :: Path.AbsDir
    hostStackPath = hostHomePath </> Path.relDir ".stack-lht"

    hostStackWorkPath :: Path.AbsDir
    hostStackWorkPath = hostProjectPath </> Path.relDir ".stack-work-lht"

    command :: CBT.Command
    command = CBT.Command
      { name = "stack"
      , arguments =
        [ "build"
        , "--allow-different-user"
        , "--copy-bins"
        , "--interleaved-output"
        , "--system-ghc"
        , "--work-dir", ".stack-work-lht"
        ] <> flagArguments <>
        [ convertText packageName <> ":" <> convertText targetName
        ]
      }

    mounts :: [CBT.Mount]
    mounts =
      [ CBT.Mount { hostPath = hostProjectPath, containerPath = containerProjectPath }
      , CBT.Mount { hostPath = hostStackPath,   containerPath = containerStackPath }
      ]

    containerDefinition =
      (CBT.minimalContainerDefinition (getField @"imageName" buildDefinition) containerName)
      { CBT.command = pure command
      , CBT.detach  = CBT.Foreground
      , CBT.env     = [CBT.EnvInherit "STACK_YAML"]
      , CBT.remove  = CBT.NoRemove
      , CBT.workDir = pure containerProjectPath
      , CBT.mounts  = mounts
      }

  setupSharedDirectory hostStackPath
  setupSharedDirectory hostStackWorkPath

  Exception.bracket_
    (CBT.buildRun buildDefinition containerDefinition)
    (CBT.removeContainer containerName)
    (action containerName)

  where
    flagArguments :: [Text]
    flagArguments = Foldable.foldMap mkFlag flags
      where
        mkFlag :: Flag -> [Text]
        mkFlag (Flag name value) =
          ["--flag", convert name <> ":" <> convert value]

setupSharedDirectory :: MonadIO m => Path.AbsDir -> m ()
setupSharedDirectory path = liftIO $ do
  Path.createDirectoryIfMissing False path
  Files.setFileMode (Path.toString path) Files.accessModes

containerHomePath :: Path.AbsDir
containerHomePath = Path.absDir "/opt/build"
