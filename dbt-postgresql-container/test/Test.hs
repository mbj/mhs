{-# LANGUAGE TemplateHaskell #-}

import MPrelude

import qualified CBT
import qualified CBT.Container
import qualified DBT.Postgresql           as Postgresql
import qualified DBT.Postgresql.Container as DBT
import qualified Data.ByteString.Lazy     as LBS
import qualified Data.Text.Encoding       as Text
import qualified Devtools
import qualified System.Process.Typed     as Process
import qualified Test.Tasty               as Tasty
import qualified Test.Tasty.MGolden       as Tasty

main :: IO ()
main = do
  CBT.runDefaultEnvironment $ do
    containerName <- CBT.Container.nextName (CBT.Container.Prefix "dbt-test")
    DBT.withDatabaseContainerDefault containerName $ \clientConfig ->
      liftIO . Tasty.defaultMain $ Tasty.testGroup "dbt"
        [ Devtools.testTree $$(Devtools.readDependencies [Devtools.Target "dbt-postgresql-container"])
        , testDB clientConfig
        ]

testDB :: Postgresql.ClientConfig -> Tasty.TestTree
testDB clientConfig
  = Tasty.goldenTest "postgresql" "test/postgresql.expected" $ do
    env <- Postgresql.getEnv clientConfig
    readText
      . Process.setEnv env
      $ Process.proc "psql" ["--no-psqlrc", "--command", "SELECT 1"]

readText :: Process.ProcessConfig stdin stdout stderr -> IO Text
readText proc = Text.decodeUtf8 . LBS.toStrict <$> Process.readProcessInterleaved_ proc
