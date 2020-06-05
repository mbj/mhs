import MPrelude
import Test.Tasty.HUnit

import qualified CBT
import qualified Devtools
import qualified System.Path as Path
import qualified Test.Tasty  as Tasty

main :: IO ()
main
  = Tasty.defaultMain
  $ Tasty.testGroup "cbt" [Devtools.testTree devtoolsConfig, image, container]

devtoolsConfig :: Devtools.Config
devtoolsConfig = Devtools.defaultConfig
  { Devtools.hlintArguments = ["-XTypeApplications"] }

image :: Tasty.TestTree
image
  = testCase "image" . void
  . CBT.buildIfAbsent
  $ CBT.fromDockerfileContents
    (CBT.Prefix "cbt-test")
    (CBT.DockerfileContent "FROM alpine")

container :: Tasty.TestTree
container
  = testCase "container" . void $ do
    containerName <- CBT.nextContainerName prefix
    CBT.withContainer
      buildDefinition
      CBT.ContainerDefinition
        { detach           = CBT.Foreground
        , imageName        = (CBT.imageName :: CBT.BuildDefinition -> CBT.ImageName) buildDefinition
        , mounts           = []
        , programArguments = []
        , programName      = "true"
        , publishPorts     = []
        , remove           = CBT.NoRemove
        , removeOnRunFail  = CBT.Remove
        , workDir          = Path.absDir "/"
        , ..
        }
      (pure ())

buildDefinition :: CBT.BuildDefinition
buildDefinition =
  CBT.fromDockerfileContents
    prefix
    (CBT.DockerfileContent "FROM alpine")

prefix :: CBT.Prefix
prefix = CBT.Prefix "cbt-test"
