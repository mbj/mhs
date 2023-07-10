{-# LANGUAGE TemplateHaskell #-}

import Control.Monad (sequence)
import PGT.Prelude
import Prelude (error)

import qualified CBT
import qualified CBT.Container
import qualified DBT.ClientConfig         as DBT
import qualified DBT.Container            as DBT
import qualified Data.Text.IO             as Text
import qualified Devtools
import qualified PGT
import qualified PGT.Output
import qualified PGT.Output.Definition
import qualified PGT.Output.Test
import qualified PGT.Output.Test.Comments
import qualified PGT.Output.Test.QueryPlan
import qualified PGT.Output.Test.Result
import qualified PGT.Output.TestSuite
import qualified PGT.Selector             as PGT
import qualified System.Path              as Path
import qualified System.Posix.Process     as Process
import qualified Test.Tasty               as Tasty
import qualified Test.Tasty.HUnit         as Tasty

main :: IO ()
main = do
  Text.putStrLn ""

  CBT.runDefaultEnvironment $ do
    containerName <- CBT.Container.nextName $ CBT.Container.Prefix "pgt"
    DBT.withDatabaseContainerDefault containerName $ \pgConfig -> do
      let clientConfig = pgConfig { DBT.databaseName = DBT.DatabaseName "template1" }
      let pgtConfig = PGT.Config{setupFile = pure $ Path.file "test/setup.sql", ..}
      pgtPid <- liftIO Process.getProcessID
      runMIO PGT.Environment{..} $ do
        PGT.withSetupDatabase $ \setupDatabase -> do
          testExamples <- PGT.testTree setupDatabase identity =<< PGT.expand selectors

          outputTestTree <-
            liftIO
              $ sequence
              [ PGT.Output.Definition.testTree
              , PGT.Output.Test.Comments.testTree
              , PGT.Output.Test.QueryPlan.testTree
              , PGT.Output.Test.Result.testTree
              , PGT.Output.Test.testTree
              , PGT.Output.TestSuite.testTree
              , PGT.Output.testTree
              ]

          liftIO . Tasty.defaultMain .
            Tasty.testGroup "" $
              [ Devtools.testTree $$(Devtools.readDependencies [Devtools.Target "pgt"])
              , testExamples
              , testSharding
              ] <> outputTestTree
  where
    selectors :: Vector PGT.Selector
    selectors =
      [ PGT.Selector $ Path.rel "examples/success.test.sql"
      , PGT.Selector $ Path.rel "examples/write-1.test.sql"
      , PGT.Selector $ Path.rel "examples/write-2.test.sql"
      ]

testSharding :: Tasty.TestTree
testSharding = Tasty.testGroup "PGT.selectShard"
  [ test "one shard, empty"                    shardConfigOne     []         []
  , test "one shard, one element"              shardConfigOne     ["A"]      ["A"]
  , test "one shard, two elements"             shardConfigOne     ["A", "B"] ["A", "B"]
  , test "two shards, index 0, empty input"    (shardConfigTwo 0) []         []
  , test "two shards, index 1, empty input"    (shardConfigTwo 1) []         []
  , test "two shards, index 0, one element"    (shardConfigTwo 0) []         ["A"]
  , test "two shards, index 1, one element"    (shardConfigTwo 1) ["A"]      ["A"]
  , test "two shards, index 0, two elements"   (shardConfigTwo 0) ["A"]      ["A", "B"]
  , test "two shards, index 1, two elements"   (shardConfigTwo 1) ["B"]      ["A", "B"]
  , test "two shards, index 0, three elements" (shardConfigTwo 0) ["A"]      ["A", "B", "C"]
  , test "two shards, index 1, three elements" (shardConfigTwo 1) ["B", "C"] ["A", "B", "C"]
  ]
  where
    test :: String -> PGT.ShardConfig -> Vector Text -> Vector Text -> Tasty.TestTree
    test name shardConfig expected input
      = Tasty.testCase name
      . Tasty.assertEqual "" expected
      $ PGT.selectShard shardConfig input

    shardConfigOne
      = either error identity
      $ PGT.parseShardConfig
        (either error identity $ PGT.parseShardCount 1)
        (PGT.ShardIndex 0)

    shardConfigTwo index
      = either error identity
      $ PGT.parseShardConfig
        (either error identity $ PGT.parseShardCount 2)
        (PGT.ShardIndex index)
