module CBT.Image
  ( Destination(..)
  , build
  , buildIfAbsent
  , isPresent
  , pull
  , push
  , tag
  , tryPull
  )
where

import CBT.Backend
import CBT.Config
import CBT.Image.BuildDefinition
import CBT.Prelude
import CBT.Proc
import Control.Monad (unless)

import qualified CBT.Image.Name       as CBT.Image
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Encoding   as Text
import qualified System.Path          as Path
import qualified System.Process.Typed as Process

newtype Destination = Destination Text
  deriving (Conversion Text) via Text

buildIfAbsent
  :: Env env
  => BuildDefinition name
  -> RIO env ()
buildIfAbsent buildDefinition@BuildDefinition{..} = do
  exists <- isPresent imageName
  unless exists $ build buildDefinition

isPresent
  :: forall name env
  . (CBT.Image.IsName name, Env env)
  => name
  -> RIO env Bool
isPresent imageName =
  arguments
    >>= fmap silenceStdout . backendProc
    >>= fmap exitBool . runProcess
  where
    arguments = askBackend <&> \case
      Docker ->
        [ "inspect"
        , "--type", "image"
        , "--"
        , imageNameString
        ]
      Podman ->
        [ "image"
        , "exists"
        , "--"
        , imageNameString
        ]

    imageNameString = CBT.Image.nameString imageName

push :: Env env => CBT.Image.QualifiedName -> RIO env ()
push imageName = runProcess_ =<< backendProc ["push", convertVia @Text imageName]

pull :: Env env => CBT.Image.QualifiedName -> RIO env ()
pull imageName = runProcess_ =<< backendProc ["pull", convertVia @Text imageName]

tryPull :: Env env => CBT.Image.QualifiedName -> RIO env Bool
tryPull imageName = fmap exitBool . runProcess =<< backendProc ["pull", convertVia @Text imageName]

tag :: (Env env, CBT.Image.IsName a, CBT.Image.IsName b) => a -> b -> RIO env ()
tag source target = runProcess_ =<< proc
  where
    proc = backendProc
      [ "tag"
      , convertVia @Text (CBT.Image.toName source)
      , convertVia @Text (CBT.Image.toName target)
      ]

build
  :: Env env
  => BuildDefinition name
  -> RIO env ()
build BuildDefinition{..}
  = runProcess_ . setVerbosity verbosity =<< proc
  where
    proc = case source of
      Directory path       -> buildProc [Path.toString path]
      Instructions content -> fromInstructions content

    fromInstructions content
      = Process.setStdin
        ( Process.byteStringInput
        . LBS.fromStrict
        . Text.encodeUtf8
        $ toText content
        )
      <$> buildProc ["-"]

    buildProc arguments
      = backendProc
      $ [ "build"
        , "--platform", "linux/amd64"
        , "--tag", CBT.Image.nameString imageName
        ]
      <> arguments

setVerbosity :: Verbosity -> Proc -> Proc
setVerbosity = \case
  Quiet -> silence
  _     -> identity

