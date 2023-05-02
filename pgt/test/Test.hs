{-# LANGUAGE TemplateHaskell #-}

import Data.Vector (Vector)
import MPrelude
import Prelude (error)

import qualified CBT
import qualified CBT.Container
import qualified DBT.Postgresql           as Postgresql
import qualified DBT.Postgresql.Container as DBT
import qualified Data.Text.IO             as Text
import qualified Devtools
import qualified PGT
import qualified PGT.Output.Definition
import qualified PGT.Selector             as PGT
import qualified System.Path              as Path
import qualified System.Process.Typed     as Process
import qualified Test.Tasty               as Tasty
import qualified Test.Tasty.HUnit         as Tasty

main :: IO ()
main = do
  Text.putStrLn ""
  success <- PGT.expand selectors

  CBT.runDefaultEnvironment $ do
    containerName <- CBT.Container.nextName $ CBT.Container.Prefix "pgt"
    DBT.withDatabaseContainerDefault containerName $ \pgConfig -> do
      let adminConfig = pgConfig { Postgresql.databaseName = Postgresql.DatabaseName "template1" }
      liftIO $ setupSchema adminConfig
      config             <- PGT.configure adminConfig empty
      definitionTestTree <- liftIO PGT.Output.Definition.testTree

      liftIO . Tasty.defaultMain .
        Tasty.testGroup "" $
          [ Devtools.testTree $$(Devtools.readDependencies [Devtools.Target "pgt"])
          , PGT.testTree config success
          , definitionTestTree
          , testSharding
          ]

setupSchema :: Postgresql.ClientConfig -> IO ()
setupSchema pgConfig = do
  env  <- Postgresql.getEnv pgConfig

  Process.runProcess_
    . Process.setEnv env
    . Process.setStdin (Process.byteStringInput "CREATE TABLE test (id serial)")
    $ Process.proc "psql" arguments

  where
    arguments :: [String]
    arguments =
      [ "--no-password"
      , "--no-psqlrc"
      , "--no-readline"
      , "--quiet"
      , "--set", "ON_ERROR_STOP=1"
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

selectors :: [PGT.Selector]
selectors =
 [ PGT.Selector $ Path.rel "examples/success.test.sql"
 , PGT.Selector $ Path.rel "examples/write-1.test.sql"
 , PGT.Selector $ Path.rel "examples/write-2.test.sql"
 ]
