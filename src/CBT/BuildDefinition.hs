module CBT.BuildDefinition
  ( fromDirectory
  , fromDockerfileContent
  , readDockerfileContent
  )
where

import CBT.Prelude
import CBT.Types
import Control.Monad.Trans.Resource (MonadResource, runResourceT)
import Data.Conduit (ConduitT, (.|), runConduit)

import qualified Crypto.Hash              as Hash
import qualified Data.ByteString          as BS
import qualified Data.Conduit             as Conduit
import qualified Data.Conduit.Combinators as Conduit
import qualified Data.Text                as Text
import qualified Data.Text.Encoding       as Text
import qualified Data.Text.IO             as Text
import qualified System.Path              as Path

readDockerfileContent
  :: MonadIO m
  => Path.AbsRelFile
  -> m DockerfileContent
readDockerfileContent path =
  DockerfileContent <$> liftIO (Text.readFile $ Path.toString path)

fromDockerfileContent
  :: Prefix
  -> DockerfileContent
  -> BuildDefinition
fromDockerfileContent prefix content =
  BuildDefinition
    { imageName = mkImageName prefix (Hash.hash . Text.encodeUtf8 $ toText content)
    , verbosity = Verbose
    , source    = Instructions content
    }

fromDirectory
  :: MonadUnliftIO m
  => Prefix
  -> Path.AbsRelDir
  -> m BuildDefinition
fromDirectory prefix dir = do
  digest <- runResourceT $ hashDirectory dir

  pure $ BuildDefinition
    { source    = Directory dir
    , imageName = mkImageName prefix digest
    , verbosity = Verbose
    }

hashDirectory
  :: forall m . MonadResource m
  => Path.AbsRelDir
  -> m (Hash.Digest Hash.SHA3_256)
hashDirectory directory = do
  Hash.hashFinalize <$> runConduit conduit
  where
    conduit
      =  Conduit.sourceDirectoryDeep False (Path.toString directory)
      .| processFile
      .| Conduit.foldl Hash.hashUpdate Hash.hashInit

    processFile :: ConduitT String BS.ByteString m ()
    processFile =
      Conduit.awaitForever $ \path ->
        addFileName path .| Conduit.sourceFile path

    addFileName :: String -> ConduitT i BS.ByteString m ()
    addFileName
      = Conduit.yield
      . (\input -> "\0" <> input <> "\0")
      . Text.encodeUtf8
      . convert

mkImageName :: Prefix -> Hash.Digest Hash.SHA3_256 -> ImageName
mkImageName prefix digest
  = ImageName $ Text.intercalate "-"
  [ convertText prefix
  , convertText $ show digest
  ]
