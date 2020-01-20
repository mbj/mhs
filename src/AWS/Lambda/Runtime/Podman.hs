module AWS.Lambda.Runtime.Podman
  ( Config(..)
  , Executable(..)
  , ImageName(..)
  , PackageName(..)
  , TargetName(..)
  , build
  )
where

import AWS.Lambda.Runtime.Executable
import AWS.Lambda.Runtime.Prelude
import Control.Monad (unless)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import System.Exit (ExitCode(ExitSuccess))
import System.Path ((</>))

import qualified AWS.Lambda.Runtime.TH as TH
import qualified Data.ByteString       as ByteString
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.List             as List
import qualified Data.Text.Encoding    as Text
import qualified Network.AWS           as AWS
import qualified Network.AWS.Data.Body as AWS
import qualified System.Path           as Path
import qualified System.Path.Directory as Path
import qualified System.Process.Typed  as Process

newtype ContainerName = ContainerName Text
  deriving newtype ToText

newtype ImageName = ImageName Text
  deriving newtype ToText

newtype PackageName = PackageName Text
  deriving newtype ToText

newtype TargetName = TargetName Text
  deriving newtype ToText

data Config = Config
  { executablePath :: Path.RelFile
  , packageName    :: PackageName
  , targetName     :: TargetName
  }

build :: forall m . MonadIO m => Config -> m Executable
build Config{..} = do
  imageBuild
  targetBuild
  mountPath <- readMountPath
  executable <-
    Executable <$> liftIO (ByteString.readFile . Path.toString $ executableHostPath mountPath)
  removeContainer
  pure executable
  where
    executableHostPath :: Path.AbsDir -> Path.AbsFile
    executableHostPath mount = mount </> containerHomePathRel </> executablePath

    containerHomePathRel :: Path.RelDir
    containerHomePathRel = Path.relDir "opt/build"

    imageBuild :: m ()
    imageBuild = do
      exists <- testImageExists imageName

      unless exists $
        Process.runProcess_
          . Process.setStdin (Process.byteStringInput dockerfile)
          $ Process.proc "podman"
            [ "build"
            , "--tag", convertText imageName
            , "--file", "-"
            ]

    targetBuild :: m ()
    targetBuild = do
      hostProjectPath <- liftIO Path.getCurrentDirectory
      hostHomePath    <- liftIO Path.getHomeDirectory

      let
        containerHomePath :: Path.AbsDir
        containerHomePath = Path.absDir "/" </> containerHomePathRel

        containerProjectPath :: Path.AbsDir
        containerProjectPath = containerHomePath </> Path.relDir (convertText packageName)

        containerStackPath :: Path.AbsDir
        containerStackPath = containerHomePath </> Path.relDir ".stack"

        hostStackPath :: Path.AbsDir
        hostStackPath = hostHomePath </> Path.relDir ".stack-lambda-runtime"

      liftIO $ Path.createDirectoryIfMissing False hostStackPath

      Process.runProcess_ $ Process.proc "podman"
        [ "run"
        , "--mount", bindMount hostProjectPath containerProjectPath
        , "--mount", bindMount hostStackPath   containerStackPath
        , "--name", convertText containerName
        , "--net", "host"
        , "--stop-timeout", "0"
        , "--tty"
        , "--workdir", Path.toString containerProjectPath
        , "--"
        , convertText imageName
        , "stack"
        , "build"
        , "--copy-bins"
        , "--flag", convertText packageName <> ":static"
        , "--interleaved-output"
        , "--system-ghc"
        , "--work-dir", ".stack-work-lambda-runtime"
        , convertText packageName <> ":" <> convertText targetName
        ]

    bindMount :: Path.AbsDir -> Path.AbsDir -> String
    bindMount source destination =
      List.intercalate
        ","
        [ "type=bind"
        , "source="      <> Path.toString source
        , "destination=" <> Path.toString destination
        ]

    readMountPath :: m Path.AbsDir
    readMountPath =
      Path.absDir . rstrip . convertText . Text.decodeUtf8 . LBS.toStrict <$> readProcess
      where
        readProcess :: m LBS.ByteString
        readProcess
          = Process.readProcessStdout_
          $ Process.proc "podman"
          [ "mount"
          , convertText containerName
          ]

        rstrip :: String -> String
        rstrip
          = List.reverse
          . List.dropWhile (== '\n')
          . List.reverse

    removeContainer :: m ()
    removeContainer =
      Process.runProcess_ $ Process.proc "podman"
        [ "rm"
        , convertText containerName
        ]

    imageName :: ImageName
    imageName =
      ImageName $
        "lambda-build-" <> (decodeUtf8 . AWS.sha256Base16 $ AWS.toHashed dockerfile)

    containerName :: ContainerName
    containerName = ContainerName $ convertText imageName

#ifndef __HLINT__
    dockerfile = LBS.fromStrict $ encodeUtf8 $$(TH.readFile "Dockerfile")
#endif

testImageExists :: MonadIO m => ImageName -> m Bool
testImageExists imageName = checkExit <$> Process.runProcess process
  where
    process = Process.proc "podman" ["image", "exists", "--", convertText imageName]

    checkExit = \case
      ExitSuccess -> True
      _           -> False
