module DBT.Image (ImageName(..), build, name, testImageExists) where

import DBT.Prelude
import Data.ByteString (ByteString)

import qualified Crypto.Hash          as Hash
import qualified DBT.TH               as TH
import qualified Data.ByteString.Lazy as LBS
import qualified System.Exit          as Exit
import qualified System.Path          as Path
import qualified System.Process.Typed as Process

newtype ImageName = ImageName Text
  deriving newtype ToText

name :: ImageName
name
  = ImageName
  . ("localhost/dbt-" <>)
  . convertText
  . show
  $ Hash.hashWith Hash.SHA3_256 dockerfile

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
  $ Process.setStdin (Process.byteStringInput $ LBS.fromStrict dockerfile)
  $ Process.proc "podman" ["build", "--tag", convertText name, "-"]

#ifndef __HLINT__
dockerfile :: ByteString
dockerfile = $$(TH.readFile $ Path.file "Dockerfile")
#endif
