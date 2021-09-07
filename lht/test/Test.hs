import MPrelude
import System.Path ((</>))

import qualified CBT
import qualified Control.Exception     as Exception
import qualified Data.Elf              as ELF
import qualified Data.Foldable         as Foldable
import qualified Devtools
import qualified LHT.Build
import qualified System.Environment    as Environment
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
    stackYamlEnv :: Path.AbsRelFile <- Path.file <$> Environment.getEnv "STACK_YAML"

    let stackYamlPath = Path.dir "./" </> Path.takeFileName stackYamlEnv

    executable <- withCurrentDirectory (Path.relDir "example") . CBT.runDefaultEnvironment $
      LHT.Build.build $ config stackYamlPath

    Tasty.assertBool
      "static binary"
      (Foldable.null . ELF.parseSymbolTables $ ELF.parseElf executable)

config :: Path.AbsRelFile -> LHT.Build.Config
config stackYamlPath = LHT.Build.Config
  { executablePath = Path.relFile ".local/bin/hello-world"
  , flags          = [LHT.Build.Flag packageName "static"]
  , packageName    = packageName
  , stackYamlPath  = pure stackYamlPath
  , targetName     = LHT.Build.TargetName "hello-world"
  }
  where
    packageName = LHT.Build.PackageName "example"

withCurrentDirectory :: Path.RelDir -> IO a -> IO a
withCurrentDirectory dir action =
  Exception.bracket Path.getCurrentDirectory Path.setCurrentDirectory . const $ do
    Path.setCurrentDirectory dir
    action
