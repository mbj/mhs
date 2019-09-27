module AWS.Lambda.Runtime.Podman
  ( Config(..)
  , ExecutablePath(..)
  , ImageName(..)
  , PackageName(..)
  , TargetName(..)
  , build
  )
where

import AWS.Lambda.Runtime.Prelude
import Control.Monad (unless)
import Data.Bits (shiftL)
import Data.ByteString (ByteString)
import Data.Foldable (foldr')
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import GHC.Real (fromIntegral, toInteger)
import System.Exit (ExitCode(ExitSuccess))
import System.FilePath (FilePath, (</>))
import System.Posix.Types (FileMode)

import qualified AWS.Lambda.Runtime.TH as TH
import qualified Codec.Archive.Zip     as Zip
import qualified Data.ByteString       as ByteString
import qualified Data.ByteString.Lazy  as ByteString (fromStrict)
import qualified Network.AWS           as AWS
import qualified Network.AWS.Data.Body as AWS
import qualified System.Directory      as Directory
import qualified System.Posix.Files    as File
import qualified System.Process.Typed  as Process

newtype ExecutablePath = ExecutablePath FilePath
  deriving newtype ToText

newtype ImageName = ImageName Text
  deriving newtype ToText

newtype PackageName = PackageName Text
  deriving newtype ToText

newtype TargetName = TargetName Text
  deriving newtype ToText

data Config = Config
  { executablePath :: ExecutablePath
  , packageName    :: PackageName
  , targetName     :: TargetName
  }

build :: forall m . MonadIO m => Config -> m ByteString
build Config{..} = do
  imageBuild
  targetBuild

  liftIO . ByteString.readFile $ convertText executablePath

  where
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
      hostProjectPath <- liftIO Directory.getCurrentDirectory
      hostHomePath    <- liftIO Directory.getHomeDirectory

      let
        buildHomePath :: FilePath
        buildHomePath = "/opt/build"

        buildProjectPath :: FilePath
        buildProjectPath = buildHomePath </> convertText packageName

        buildStackPath :: FilePath
        buildStackPath = buildHomePath </> ".stack"

        hostStackPath :: FilePath
        hostStackPath = hostHomePath </> ".stack-lambda-runtime"

      liftIO $ Directory.createDirectoryIfMissing False hostStackPath

      Process.runProcess_ $ Process.proc "podman"
        [ "run"
        , "--mount", "type=bind,source=" <> hostProjectPath <> ",destination=" <> buildProjectPath
        , "--mount", "type=bind,source=" <> hostStackPath   <> ",destination=" <> buildStackPath
        , "--net", "host"
        , "--rm"
        , "--stop-timeout", "0"
        , "--tty"
        , "--workdir", buildProjectPath
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

    imageName :: ImageName
    imageName =
      ImageName $
        "lambda-build-" <> (decodeUtf8 . AWS.sha256Base16 $ AWS.toHashed dockerfile)

#ifndef __HLINT__
    dockerfile = ByteString.fromStrict $ encodeUtf8 $$(TH.readFile "Dockerfile")
#endif

functionArchive :: ByteString -> Zip.Archive
functionArchive bootstrap = Zip.addEntryToArchive bootstrapEntry Zip.emptyArchive
  where
    bootstrapEntry =
      setMode
        bootstrapFileMode
        (Zip.toEntry "bootstrap" 0 $ ByteString.fromStrict bootstrap)

    bootstrapFileMode =
      foldr'
        File.unionFileModes
        File.regularFileMode
        ([File.otherExecuteMode, File.otherReadMode] :: [FileMode])

setMode :: FileMode -> Zip.Entry -> Zip.Entry
setMode newMode entry = entry
  { Zip.eExternalFileAttributes = fromIntegral (shiftL (toInteger newMode) 16)
  , Zip.eVersionMadeBy          = 0x0300  -- UNIX file attributes
  }

testImageExists :: MonadIO m => ImageName -> m Bool
testImageExists imageName = checkExit <$> Process.runProcess process
  where
    process = Process.proc "podman" ["image", "exists", "--", convertText imageName]

    checkExit = \case
      ExitSuccess -> True
      _           -> False
