import MPrelude

import qualified CBT
import qualified Control.Exception     as Exception
import qualified Devtools
import qualified LHT.Build
import qualified System.Path           as Path
import qualified System.Path.Directory as Path
import qualified Test.Tasty            as Tasty
import qualified Test.Tasty.HUnit      as Tasty

main :: IO ()
main = do
  devtools <- Devtools.testTree Devtools.defaultConfig { Devtools.targets = [Devtools.Target "lht"] }

  Tasty.defaultMain $ Tasty.testGroup "lht"
    [ testBuild
    , Tasty.after Tasty.AllFinish "test-build" devtools
    ]

testBuild :: Tasty.TestTree
testBuild =
  Tasty.testCase "test-build" $ do
    void $ withCurrentDirectory (Path.relDir "example") . CBT.runDefaultEnvironment $
      LHT.Build.build config

config :: LHT.Build.Config
config = LHT.Build.Config
  { cbtBuildDefinition = LHT.Build.defaultCBTBuildDefinition
  , executablePath     = Path.relFile ".local/bin/hello-world"
  , flags              = [LHT.Build.Flag packageName "lht"]
  , packageName        = packageName
  , targetName         = LHT.Build.TargetName "hello-world"
  }
  where
    packageName = LHT.Build.PackageName "example"

withCurrentDirectory :: Path.RelDir -> IO a -> IO a
withCurrentDirectory dir action =
  Exception.bracket Path.getCurrentDirectory Path.setCurrentDirectory . const $ do
    Path.setCurrentDirectory dir
    action
