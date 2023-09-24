{-# LANGUAGE TemplateHaskell #-}

import MPrelude

import qualified CBT
import qualified CBT.Container
import qualified DBT.ClientConfig     as DBT
import qualified DBT.Connection       as DBT
import qualified DBT.Container        as DBT
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Encoding   as Text
import qualified Devtools
import qualified System.Process.Typed as Process
import qualified Test.Tasty           as Tasty
import qualified Test.Tasty.HUnit     as Tasty
import qualified Test.Tasty.MGolden   as Tasty

main :: IO ()
main = do
  CBT.runDefaultEnvironment $ do
    containerName <- CBT.Container.nextName (CBT.Container.Prefix "dbt-test")
    DBT.withDatabaseContainerDefault containerName $ \clientConfig ->
      liftIO . Tasty.defaultMain $ Tasty.testGroup "dbt"
        [ Devtools.testTree $$(Devtools.readDependencies [Devtools.Target "dbt"])
        , testDB clientConfig
        , Tasty.testGroup "toSettings"
          [ Tasty.testCase "minimal"
          . Tasty.assertEqual ""
            "dbname='test_database' host='test.example.com' port='5432' user='test_user_name'"
          $ DBT.toSettings defaultConfig
          , Tasty.testCase "maximal"
          . Tasty.assertEqual ""
            "dbname='test_database' host='test.example.com' port='5555' user='test_user_name' password='test-password' sslmode='verify-full' sslrootcert='system'"
          $ DBT.toSettings
            defaultConfig
            { DBT.hostPort    = pure $ DBT.HostPort 5555
            , DBT.password    = pure $ DBT.Password "test-password"
            , DBT.sslMode     = pure $ DBT.SSLMode "verify-full"
            , DBT.sslRootCert = pure $ DBT.SSLRootCert "system"
            }
          , Tasty.testCase "escapes"
          . Tasty.assertEqual ""
            "dbname='test_database' host='test.example.com' port='5555' user='test_user_name' password='test-password\\'' sslmode='verify-full\\'' sslrootcert='system\\''"
          $ DBT.toSettings
            defaultConfig
            { DBT.hostPort    = pure $ DBT.HostPort 5555
            , DBT.password    = pure $ DBT.Password "test-password'"
            , DBT.sslMode     = pure $ DBT.SSLMode "verify-full'"
            , DBT.sslRootCert = pure $ DBT.SSLRootCert "system'"
            }
          ]
        ]

defaultConfig :: DBT.ClientConfig
defaultConfig =
  DBT.ClientConfig
    { databaseName = DBT.DatabaseName "test_database"
    , hostName     = DBT.HostName "test.example.com"
    , hostPort     = empty
    , password     = empty
    , sslMode      = empty
    , sslRootCert  = empty
    , userName     = DBT.UserName "test_user_name"
    }

testDB :: DBT.ClientConfig -> Tasty.TestTree
testDB clientConfig
  = Tasty.goldenTest "postgresql" "test/postgresql.expected" $ do
    env <- DBT.getEnv clientConfig
    readText
      . Process.setEnv env
      $ Process.proc "psql" ["--no-psqlrc", "--command", "SELECT 1"]

readText :: Process.ProcessConfig stdin stdout stderr -> IO Text
readText proc = Text.decodeUtf8 . LBS.toStrict <$> Process.readProcessInterleaved_ proc
