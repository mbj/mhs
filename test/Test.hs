import MPrelude
import System.IO (IO)

import qualified Data.Elf             as ELF
import qualified Data.Foldable        as Foldable
import qualified Devtools
import qualified LHT.Build
import qualified System.Path          as Path
import qualified Test.Tasty           as Tasty
import qualified Test.Tasty.HUnit     as Tasty

main :: IO ()
main =
  Tasty.defaultMain $ Tasty.testGroup "lht"
    [ testBuild
    , Devtools.testTree Devtools.defaultConfig
    ]

testBuild :: Tasty.TestTree
testBuild
  = Tasty.testCase "test-build" $ do
   executable <- LHT.Build.un <$> LHT.Build.build config

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
    packageName = LHT.Build.PackageName "lht"
