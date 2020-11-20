import GHC.Records (getField)
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
  devtools <- Devtools.testTree devtoolsConfig
  Tasty.defaultMain
    $ Tasty.testGroup "cbt"
    [ container cbt
    , devtools
    , imageDirectory cbt
    , imageStatic cbt
    ]

devtoolsConfig :: Devtools.Config
devtoolsConfig = Devtools.defaultConfig
  { Devtools.hlintArguments = ["-XTypeApplications"] }

imageDirectory :: CBT.Environment -> Tasty.TestTree
imageDirectory cbt
  = testCase "image from directory definition"
  . void
  . runRIO cbt
  $ CBT.buildIfAbsent =<< CBT.fromDirectory prefix (Path.dir "test/app")

imageStatic :: CBT.Environment -> Tasty.TestTree
imageStatic cbt
  = testCase "image from static definition"
  . void
  . runRIO cbt
  $ CBT.buildIfAbsent buildDefinitionStatic

container :: CBT.Environment -> Tasty.TestTree
container cbt
  = testCase "container" . void $
    runRIO cbt $ testRun buildDefinitionStatic

testRun :: CBT.WithEnv env => CBT.BuildDefinition -> RIO env ()
testRun buildDefinition = do
  containerName <- CBT.nextContainerName prefix

  CBT.withContainerBuildRun
    buildDefinition
    CBT.ContainerDefinition
      { detach           = CBT.Foreground
      , imageName        = getField @"imageName" buildDefinition
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

buildDefinitionStatic :: CBT.BuildDefinition
buildDefinitionStatic =
  CBT.fromDockerfileContent
    prefix
    (CBT.DockerfileContent "FROM alpine")

prefix :: CBT.Prefix
prefix = CBT.Prefix "cbt-test"
