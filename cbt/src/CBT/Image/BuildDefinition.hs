{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE GADTs #-}

module CBT.Image.BuildDefinition
  ( BuildArgument(..)
  , BuildDefinition(..)
  , BuildSource(..)
  , DockerfileContent(..)
  , Verbosity(..)
  , fromDirectory
  , fromDockerfileContent
  , hashContentTag
  , readDockerfileContent
  )
where

import CBT.Prelude
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Data.Conduit (ConduitT, (.|), runConduit)
import Language.Haskell.TH.Syntax (Lift)
import System.FilePath (FilePath)

import qualified CBT.Image.Name           as CBT.Image
import qualified Crypto.Hash              as Hash
import qualified Data.ByteString          as BS
import qualified Data.Conduit             as Conduit
import qualified Data.Conduit.Combinators as Conduit
import qualified Data.Conduit.List        as Conduit
import qualified Data.Text.Encoding       as Text
import qualified Data.Text.IO             as Text
import qualified System.Directory         as System
import qualified System.Path              as Path

newtype Prefix = Prefix Text
  deriving (Conversion Text) via Text

data Verbosity = Verbose | Quiet

data BuildSource
  = Directory Path.AbsRelDir
  | Instructions DockerfileContent

data BuildArgument = BuildArgument
  { name  :: Text
  , value :: Text
  }

data BuildDefinition name where
  BuildDefinition
    :: CBT.Image.IsName name
    => { buildArguments :: [BuildArgument]
       , source         :: BuildSource
       , imageName      :: name
       , verbosity      :: Verbosity
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
    { buildArguments = []
    , imageName      = CBT.Image.setTag imageName tag
    , source         = Instructions content
    , verbosity      = Verbose
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

hashContentTag :: MonadUnliftIO m => [FilePath] -> m CBT.Image.Tag
hashContentTag = fmap (CBT.Image.Tag . convertText . show) . runResourceT . hashContents

hashContents :: forall m . MonadUnliftIO m => [FilePath] -> m (Hash.Digest Hash.SHA3_256)
hashContents contents = runResourceT $ Hash.hashFinalize <$> runConduit conduit
  where
    conduit
      =  Conduit.sourceList contents
      .| expand
      .| readFile
      .| Conduit.foldl Hash.hashUpdate Hash.hashInit

    expand :: ConduitT FilePath FilePath (ResourceT m) ()
    expand = Conduit.awaitForever $ \path -> do
      isDirectory <- liftIO $ System.doesDirectoryExist path
      if isDirectory
        then Conduit.sourceDirectoryDeep False path
        else Conduit.yield path

    readFile
      = Conduit.awaitForever
      $ \path -> addFileName path .| Conduit.sourceFile path

    addFileName :: String -> ConduitT i BS.ByteString (ResourceT m) ()
    addFileName
      = Conduit.yield
      . (\input -> "\0" <> input <> "\0")
      . Text.encodeUtf8
      . convert

fromDirectory
  :: MonadUnliftIO m
  => CBT.Image.TaglessName
  -> Path.AbsRelDir
  -> m (BuildDefinition CBT.Image.TaggedName)
fromDirectory imageName dir = do
  tag <- hashContentTag [Path.toString dir]

  pure $ BuildDefinition
    { buildArguments = []
    , imageName      = CBT.Image.setTag imageName tag
    , source         = Directory dir
    , verbosity      = Verbose
    }
