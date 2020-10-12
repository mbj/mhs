import MPrelude
import MRIO.Core
import Test.Tasty.HUnit

import qualified CBT
import qualified Devtools
import qualified System.Path as Path
import qualified Test.Tasty  as Tasty

main :: IO ()
main = do
  cbt <- CBT.newDefaultEnvironment
  Tasty.defaultMain
    $ Tasty.testGroup "cbt"
    [ Devtools.testTree devtoolsConfig
    , container cbt
    , image cbt
    ]

devtoolsConfig :: Devtools.Config
devtoolsConfig = Devtools.defaultConfig
  { Devtools.hlintArguments = ["-XTypeApplications"] }

image :: CBT.Environment -> Tasty.TestTree
image cbt
  = testCase "image" . void
  . runRIO cbt
  . CBT.buildIfAbsent
  $ CBT.fromDockerfileContents
    (CBT.Prefix "cbt-test")
    (CBT.DockerfileContent "FROM alpine")

container :: CBT.Environment -> Tasty.TestTree
container cbt
  = testCase "container" . void $
    runRIO cbt $ do
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
