import MPrelude
import System.IO (IO)
import Test.Tasty.HUnit

import qualified DBT.Podman as Podman
import qualified Devtools
import qualified Test.Tasty as Tasty

main :: IO ()
main
  = Tasty.defaultMain
  $ Tasty.testGroup "dbt" [Devtools.testTree devtoolsConfig, image]

devtoolsConfig :: Devtools.Config
devtoolsConfig = Devtools.defaultConfig
  { Devtools.hlintArguments = ["-XTypeApplications"] }

image :: Tasty.TestTree
image
  = testCase "image" . void $ Podman.getImage
