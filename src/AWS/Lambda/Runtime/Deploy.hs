module AWS.Lambda.Runtime.Deploy
  ( Config(..)
  , ExecutablePath(..)
  , ImageName(..)
  , PackageName(..)
  , TargetName(..)
  , TargetObject(..)
  , getFunctionTarget
  , syncTarget
  )
where

import AWS.Lambda.Runtime.Prelude
import Codec.Archive.Zip
import Control.Monad (unless)
import Control.Monad.Catch (catchIf)
import Control.Monad.Trans.AWS (AWSConstraint)
import Data.Bits (shiftL)
import Data.ByteString (ByteString, readFile)
import Data.ByteString.Lazy (fromStrict)
import Data.Foldable (foldr')
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import GHC.Real (fromIntegral, toInteger)
import System.Exit (ExitCode(ExitSuccess))
import System.FilePath (FilePath, (</>))
import System.Posix.Files
import System.Posix.Types
import System.Process.Typed

import qualified AWS.Lambda.Runtime.TH     as TH
import qualified Data.Text.IO              as Text
import qualified Network.AWS               as AWS
import qualified Network.AWS.Data.Body     as AWS
import qualified Network.AWS.S3.HeadObject as S3
import qualified Network.AWS.S3.PutObject  as S3
import qualified Network.AWS.S3.Types      as S3
import qualified Network.HTTP.Types        as HTTP
import qualified System.Directory          as Directory

newtype ExecutablePath = ExecutablePath FilePath
  deriving newtype ToText

newtype ImageName = ImageName Text
  deriving newtype ToText

newtype PackageName = PackageName Text
  deriving newtype ToText

newtype TargetName = TargetName Text
  deriving newtype ToText

data Config = Config
  { bucketName     :: S3.BucketName
  , executablePath :: ExecutablePath
  , packageName    :: PackageName
  , targetName     :: TargetName
  }

data TargetObject = TargetObject
  { bucketName    :: S3.BucketName
  , message       :: Text
  , object        :: AWS.HashedBody
  , objectKey     :: S3.ObjectKey
  , objectKeyText :: Text
  }

syncTarget :: (AWSConstraint r m, AWS.MonadAWS m) => TargetObject -> m ()
syncTarget TargetObject{..} =
  putIfAbsent bucketName objectKey object (liftIO $ Text.putStrLn message)

getFunctionTarget :: forall m . MonadIO m => Config -> m TargetObject
getFunctionTarget Config{..} = do
  imageBuild
  targetBuild

  bootstrap <- liftIO (readFile $ convertText executablePath)

  let
    object        = AWS.toHashed . fromArchive $ functionArchive bootstrap
    objectKeyText = decodeUtf8 (AWS.sha256Base16 object) <> ".zip"
    objectKey     = S3.ObjectKey objectKeyText

  pure TargetObject
    { message = "Uploading new lambda function: " <> objectKeyText
    , ..
    }

  where
    imageBuild :: m ()
    imageBuild = do
      exists <- testImageExists imageName

      unless exists $
        runProcess_
          . setStdin (byteStringInput dockerfile)
          $ proc "podman"
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

      runProcess_ $ proc "podman"
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
    dockerfile = fromStrict $ encodeUtf8 $$(TH.readFile "Dockerfile")
#endif

functionArchive :: ByteString -> Archive
functionArchive bootstrap = addEntryToArchive bootstrapEntry emptyArchive
  where
    bootstrapEntry =
      setMode
        bootstrapFileMode
        (toEntry "bootstrap" 0 $ fromStrict bootstrap)

    bootstrapFileMode =
      foldr'
        unionFileModes
        regularFileMode
        ([otherExecuteMode, otherReadMode] :: [FileMode])

setMode :: FileMode -> Entry -> Entry
setMode newMode entry = entry
  { eExternalFileAttributes = fromIntegral (shiftL (toInteger newMode) 16)
  , eVersionMadeBy          = 0x0300  -- UNIX file attributes
  }

testImageExists :: MonadIO m => ImageName -> m Bool
testImageExists imageName = checkExit <$> runProcess process
  where
    process = proc "podman" ["image", "exists", "--", convertText imageName]

    checkExit = \case
      ExitSuccess -> True
      _           -> False

testObjectExists
  :: (AWSConstraint r m, AWS.MonadAWS m)
  => S3.BucketName
  -> S3.ObjectKey
  -> m Bool
testObjectExists bucketName objectKey =
  catchIf isNotFoundError
    ((void . AWS.send $ S3.headObject bucketName objectKey) >> pure True)
    (const $ pure False)
  where
    isNotFoundError
      ( AWS.ServiceError
        AWS.ServiceError'
        { _serviceStatus = HTTP.Status { HTTP.statusCode = 404 } }
      )
      = True

    isNotFoundError _ = False

putIfAbsent
  :: (AWSConstraint r m, AWS.MonadAWS m)
  => S3.BucketName
  -> S3.ObjectKey
  -> AWS.HashedBody
  -> m ()
  -> m ()
putIfAbsent bucketName objectKey object callback = do
  exists <- testObjectExists bucketName objectKey

  unless exists $ do
    callback
    void . AWS.send $ S3.putObject bucketName objectKey (AWS.Hashed object)
