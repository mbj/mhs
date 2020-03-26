module DBT.Image (ImageName(..), hbaSource, imageName, testImageExists) where

import DBT.Prelude
import Data.ByteString (ByteString)

import qualified Crypto.Hash          as Hash
import qualified DBT.TH               as TH
import qualified System.Exit          as Exit
import qualified System.Path          as Path
import qualified System.Process.Typed as Process

newtype ImageName = ImageName Text
  deriving newtype ToText

imageName :: ImageName
imageName
  = ImageName
  . ("dbt-" <>)
  . convertText
  . show
  . Hash.hashFinalize
  $ Hash.hashUpdates
    (Hash.hashInit :: Hash.Context Hash.SHA3_256)
    [buildSource, hbaSource]
#ifndef __HLINT__
  where
    buildSource = $$(TH.readFile $ Path.file "src/DBT/Build.hs")
#endif

#ifndef __HLINT__
hbaSource :: ByteString
hbaSource = $$(TH.readFile $ Path.file "pg_hba.conf")
#endif

testImageExists :: MonadIO m => m Bool
testImageExists = checkExit <$> Process.runProcess process
  where
    process =
      Process.proc
        "podman"
        [ "image"
        , "exists"
        , "--"
        , convertText imageName
        ]

    checkExit = \case
      Exit.ExitSuccess -> True
      _                -> False
