module CBT.BuildDefinition where

import CBT.Prelude
import CBT.Types

import qualified Crypto.Hash        as Hash
import qualified Data.Text          as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO       as Text
import qualified System.Path        as Path

readDockerfile :: MonadIO m => Prefix -> Path.AbsRelFile -> m BuildDefinition
readDockerfile prefix path =
  fromDockerfileContents prefix . DockerfileContent <$> liftIO (Text.readFile $ Path.toString path)

fromDockerfileContents
  :: Prefix
  -> DockerfileContent
  -> BuildDefinition
fromDockerfileContents prefix content =
  BuildDefinition
    { imageName = ImageName $ Text.intercalate "-"
      [ convertText prefix
      , convertText . show . Hash.hashWith Hash.SHA3_256 . Text.encodeUtf8 $ toText content
      ]
    , verbosity = Quiet
    , ..
    }
