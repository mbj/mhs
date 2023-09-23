module LHT.Build
  ( Config(..)
  , Flag(..)
  , PackageName(..)
  , TargetName(..)
  , build
  , buildZip
  , defaultCBTBuildDefinition
  )
where

import LHT.Prelude
import System.Path ((</>))

import qualified CBT
import qualified CBT.Container
import qualified CBT.Image.BuildDefinition as CBT.Image
import qualified CBT.Image.Name            as CBT.Image
import qualified CBT.TH
import qualified Data.ByteString           as BS
import qualified Data.Elf                  as ELF
import qualified Data.Foldable             as Foldable
import qualified LHT.Zip                   as Zip
import qualified System.Environment        as Environment
import qualified System.Path               as Path
import qualified System.Path.Directory     as Path
import qualified System.Posix.Files        as Files
import qualified UnliftIO.Exception        as Exception

newtype PackageName = PackageName Text
  deriving (Conversion Text) via Text

newtype TargetName = TargetName Text
  deriving (Conversion Text) via Text

data Flag = Flag PackageName Text

data Config = Config
  { cbtBuildDefinition             :: CBT.Image.BuildDefinition CBT.Image.TaggedName
  , executablePath                 :: Path.RelFile
  , extraArchiveFiles              :: [(Path.RelFile, BS.ByteString)]
  , extraContainerBackendArguments :: [Text]
  , extraStackArguments            :: [Text]
  , flags                          :: [Flag]
  , packageName                    :: PackageName
  , targetName                     :: TargetName
  }

defaultCBTBuildDefinition :: CBT.Image.BuildDefinition CBT.Image.TaggedName
defaultCBTBuildDefinition
  =  CBT.Image.fromDockerfileContent (CBT.Image.mkLocalName "lht")
  $$(CBT.TH.readDockerfileContent $ Path.file "Dockerfile")

buildZip
  :: CBT.Env env
  => Config
  -> MIO env BS.ByteString
buildZip config = fmap (convert . Zip.mkZip config.extraArchiveFiles) (build config)

build
  :: CBT.Env env
  => Config
  -> MIO env BS.ByteString
build config@Config{..} =
  withBuildContainer config $ \containerName ->
    assertStatic =<< CBT.Container.readFile containerName (containerHomePath </> executablePath)

assertStatic
  :: BS.ByteString
  -> MIO env BS.ByteString
assertStatic executable =
  if Foldable.null . ELF.parseSymbolTables $ ELF.parseElf executable
    then pure executable
    else Exception.throwString "LHT.Build did not produce a static executable"

withBuildContainer
  :: CBT.Env env
  => Config
  -> (CBT.Container.Name -> MIO env a)
  -> MIO env a
withBuildContainer Config{..} action = do
  containerName   <- CBT.Container.nextName (CBT.Container.Prefix "lht")
  hostProjectPath <- liftIO Path.getCurrentDirectory
  hostHomePath    <- liftIO Path.getHomeDirectory

  envStackYaml :: Maybe Path.AbsRelFile <- fmap Path.file <$> liftIO (Environment.lookupEnv "STACK_YAML")

  let
    containerProjectPath :: Path.AbsDir
    containerProjectPath = containerHomePath </> Path.relDir (convertText packageName)

    containerStackPath :: Path.AbsDir
    containerStackPath = containerHomePath </> Path.relDir ".stack"

    hostStackPath :: Path.AbsDir
    hostStackPath = hostHomePath </> Path.relDir ".stack-lht"

    hostStackWorkPath :: Path.AbsDir
    hostStackWorkPath = hostProjectPath </> Path.relDir ".stack-work-lht"

    command :: CBT.Container.Entrypoint
    command = CBT.Container.Entrypoint
      { name = "stack"
      , arguments =
        [ "build"
        , "--allow-different-user"
        , "--copy-bins"
        , "--ghc-build", "lht"
        , "--interleaved-output"
        , "--work-dir", ".stack-work-lht"
        ]
        <> flagArguments
        <> [convertText packageName <> ":" <> convertText targetName]
        <> extraStackArguments
      }

    mounts :: [CBT.Container.Mount]
    mounts =
      [ CBT.Container.Mount { hostPath = hostProjectPath, containerPath = containerProjectPath }
      , CBT.Container.Mount { hostPath = hostStackPath,   containerPath = containerStackPath   }
      ]

    containerDefinition =
      (CBT.Container.minimalDefinition cbtBuildDefinition.imageName containerName)
      { CBT.Container.command               = pure command
      , CBT.Container.detach                = CBT.Container.Foreground
      , CBT.Container.env                   = Foldable.toList
        $ CBT.Container.EnvSet "STACK_YAML"
        . convert
        . Path.toString
        . Path.takeFileName <$> envStackYaml
      , CBT.Container.extraBackendArguments = extraContainerBackendArguments
      , CBT.Container.stopRemove            = CBT.Container.StopNoRemove
      , CBT.Container.workDir               = pure containerProjectPath
      , CBT.Container.mounts                = mounts
      }

  setupSharedDirectory hostStackPath
  setupSharedDirectory hostStackWorkPath

  CBT.Container.withBuildRun
    cbtBuildDefinition
    containerDefinition
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
