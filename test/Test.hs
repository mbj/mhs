import MPrelude

import qualified Control.Exception     as Exception
import qualified Data.Elf              as ELF
import qualified Data.Foldable         as Foldable
import qualified Devtools
import qualified LHT.Build
import qualified System.Path           as Path
import qualified System.Path.Directory as Path
import qualified Test.Tasty            as Tasty
import qualified Test.Tasty.HUnit      as Tasty

main :: IO ()
main =
  Tasty.defaultMain $ Tasty.testGroup "lht"
    [ testBuild
    , Tasty.after Tasty.AllFinish "test-build" $ Devtools.testTree Devtools.defaultConfig
    ]

testBuild :: Tasty.TestTree
testBuild
  = Tasty.testCase "test-build" $ do
   executable <- withCurrentDirectory (Path.relDir "example") $
     LHT.Build.un <$> LHT.Build.build config

   Tasty.assertBool
     "static binary"
     (Foldable.null . ELF.parseSymbolTables $ ELF.parseElf executable)

config :: LHT.Build.Config
config = LHT.Build.Config
  { executablePath = Path.relFile ".local/bin/hello-world"
  , packageName    = packageName
  , targetName     = LHT.Build.TargetName "hello-world"
  , flags          = [LHT.Build.Flag packageName "static"]
  }
  where
    packageName = LHT.Build.PackageName "example"

withCurrentDirectory :: Path.RelDir -> IO a -> IO a
withCurrentDirectory dir action =
  Exception.bracket Path.getCurrentDirectory Path.setCurrentDirectory . const $ do
    Path.setCurrentDirectory dir
    action
