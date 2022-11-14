{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE GADTs #-}

module CBT.Image.BuildDefinition
  ( BuildDefinition(..)
  , BuildSource(..)
  , DockerfileContent(..)
  , Verbosity(..)
  , fromDirectory
  , fromDockerfileContent
  , readDockerfileContent
  )
where

import CBT.Prelude
import Control.Monad.Trans.Resource (MonadResource, runResourceT)
import Data.Conduit (ConduitT, (.|), runConduit)
import Language.Haskell.TH.Syntax (Lift)

import qualified CBT.Image.Name           as CBT.Image
import qualified Crypto.Hash              as Hash
import qualified Data.ByteString          as BS
import qualified Data.Conduit             as Conduit
import qualified Data.Conduit.Combinators as Conduit
import qualified Data.Text.Encoding       as Text
import qualified Data.Text.IO             as Text
import qualified System.Path              as Path

newtype Prefix = Prefix Text
  deriving (Conversion Text) via Text

data Verbosity = Verbose | Quiet

data BuildSource
  = Directory Path.AbsRelDir
  | Instructions DockerfileContent

data BuildDefinition name where
  BuildDefinition
    :: CBT.Image.IsName name
    => { source    :: BuildSource
       , imageName :: name
       , verbosity :: Verbosity
       }
    -> BuildDefinition name

newtype DockerfileContent = DockerfileContent Text
  deriving (Conversion Text) via Text
  deriving stock Lift

readDockerfileContent
  :: MonadIO m
  => Path.AbsRelFile
  -> m DockerfileContent
readDockerfileContent path =
  DockerfileContent <$> liftIO (Text.readFile $ Path.toString path)

fromDockerfileContent
  :: CBT.Image.TaglessName
  -> DockerfileContent
  -> BuildDefinition CBT.Image.TaggedName
fromDockerfileContent imageName content =
  BuildDefinition
    { imageName = CBT.Image.setTag imageName tag
    , source    = Instructions content
    , verbosity = Verbose
    , ..
    }
  where
    tag
      = CBT.Image.Tag
      . convert
      . show
      . (Hash.hash :: BS.ByteString -> Hash.Digest Hash.SHA3_256)
      . Text.encodeUtf8
      $ toText content

fromDirectory
  :: MonadUnliftIO m
  => CBT.Image.TaglessName
  -> Path.AbsRelDir
  -> m (BuildDefinition CBT.Image.TaggedName)
fromDirectory imageName dir = do
  tag <- CBT.Image.Tag . convertText . show <$> runResourceT (hashDirectory dir)

  pure $ BuildDefinition
    { imageName = CBT.Image.setTag imageName tag
    , source    = Directory dir
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
