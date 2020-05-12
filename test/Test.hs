import MPrelude
import Test.Tasty.HUnit

import qualified CBT
import qualified Devtools
import qualified Test.Tasty as Tasty

main :: IO ()
main
  = Tasty.defaultMain
  $ Tasty.testGroup "cbt" [Devtools.testTree devtoolsConfig, image]

devtoolsConfig :: Devtools.Config
devtoolsConfig = Devtools.defaultConfig
  { Devtools.hlintArguments = ["-XTypeApplications"] }

image :: Tasty.TestTree
image
  = testCase "image" . void
  . CBT.buildIfAbsent
  $ CBT.fromDockerfileContents (CBT.Prefix "cbt-test") (CBT.DockerfileContent "FROM alpine")
