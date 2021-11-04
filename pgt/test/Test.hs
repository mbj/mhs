import MPrelude

import qualified CBT
import qualified DBT.Postgresql           as Postgresql
import qualified DBT.Postgresql.Container as DBT
import qualified Data.Text.IO             as Text
import qualified Devtools
import qualified PGT
import qualified PGT.Selector             as PGT
import qualified System.Path              as Path
import qualified System.Process.Typed     as Process
import qualified Test.Tasty               as Tasty

main :: IO ()
main = do
  Text.putStrLn ""
  success <- PGT.expand selectors

  CBT.runDefaultEnvironment $ do
    containerName <- CBT.nextContainerName $ CBT.Prefix "pgt"
    DBT.withDatabaseContainer containerName $ \pgConfig -> do
      let adminConfig = pgConfig { Postgresql.databaseName = Postgresql.DatabaseName "template1" }
      liftIO $ setupSchema adminConfig
      config   <- PGT.configure adminConfig empty
      devtools <- liftIO $ Devtools.testTree Devtools.defaultConfig
         { Devtools.targets = [Devtools.Target "pgt"] }
      liftIO . Tasty.defaultMain $
        Tasty.testGroup ""
          [ devtools
          , PGT.testTree config success
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

selectors :: [PGT.Selector]
selectors =
 [ PGT.Selector $ Path.rel "examples/success.test.sql"
 , PGT.Selector $ Path.rel "examples/write-1.test.sql"
 , PGT.Selector $ Path.rel "examples/write-2.test.sql"
 ]
