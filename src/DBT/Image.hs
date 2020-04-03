module DBT.Image (Name(..), build, dockerfileContents, name, testImageExists) where

import DBT.Prelude
import Data.ByteString (ByteString)

import qualified Crypto.Hash          as Hash
import qualified DBT.TH               as TH
import qualified Data.ByteString.Lazy as LBS
import qualified System.Exit          as Exit
import qualified System.Path          as Path
import qualified System.Process.Typed as Process

newtype Name = Name Text
  deriving newtype ToText

name :: Name
name
  = Name
  . ("dbt-" <>)
  . convertText
  . show
  $ Hash.hashWith Hash.SHA3_256 dockerfileContents

testImageExists :: MonadIO m => m Bool
testImageExists = checkExit <$> Process.runProcess process
  where
    process =
      Process.proc
        "podman"
        [ "image"
        , "exists"
        , "--"
        , convertText name
        ]

    checkExit = \case
      Exit.ExitSuccess -> True
      _                -> False

build :: forall m . MonadIO m => m ()
build
  = Process.runProcess_
  $ Process.setStdin (Process.byteStringInput $ LBS.fromStrict dockerfileContents)
  $ Process.proc "podman" ["build", "--tag", convertText name, "-"]

#ifndef __HLINT__
dockerfileContents :: ByteString
dockerfileContents = $$(TH.readFile $ Path.file "Dockerfile")
#endif
