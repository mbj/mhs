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
  => BuildDefinition
  -> RIO env ()
buildIfAbsent buildDefinition = do
  exists <- isPresent (getField @"imageName" buildDefinition)
  unless exists $ build buildDefinition

isPresent :: Env env => CBT.Image.TaggedName -> RIO env Bool
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

    imageNameString = convertVia @Text imageName

push :: Env env => CBT.Image.QualifiedName -> RIO env ()
push imageName = runProcess_ =<< backendProc ["push", convertVia @Text imageName]

pull :: Env env => CBT.Image.QualifiedName -> RIO env ()
pull imageName = runProcess_ =<< backendProc ["pull", convertVia @Text imageName]

tryPull :: Env env => CBT.Image.QualifiedName -> RIO env Bool
tryPull imageName = fmap exitBool . runProcess =<< backendProc ["pull", convertVia @Text imageName]

tag :: Env env => CBT.Image.Name -> CBT.Image.Name -> RIO env ()
tag source target = runProcess_ =<< backendProc ["tag", convertVia @Text source, convertVia @Text target]

build
  :: Env env
  => BuildDefinition
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
        , "--tag", convertVia @Text imageName
        ]
      <> arguments

setVerbosity :: Verbosity -> Proc -> Proc
setVerbosity = \case
  Quiet -> silence
  _     -> identity

