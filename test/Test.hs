import MPrelude
import Test.Tasty.HUnit

import qualified CBT
import qualified DBT
import qualified Devtools
import qualified System.Process.Typed as Process
import qualified Test.Tasty           as Tasty

main :: IO ()
main
  = Tasty.defaultMain
  $ Tasty.testGroup "dbt" [Devtools.testTree devtoolsConfig, postgres]

devtoolsConfig :: Devtools.Config
devtoolsConfig = Devtools.defaultConfig
  { Devtools.hlintArguments = ["-XTypeApplications"] }

postgres :: Tasty.TestTree
postgres
  = testCase "postgres" . void
  . DBT.withDatabaseEnv (CBT.Prefix "dbt")
  $ Process.proc "psql" ["-c", "SELECT 1"]
