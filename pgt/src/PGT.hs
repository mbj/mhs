module PGT
  ( Command(..)
  , Config(..)
  , Environment(..)
  , ShardConfig
  , ShardCount
  , ShardIndex(..)
  , Test(..)
  , Tests
  , defaultShardCount
  , defaultShardIndex
  , fromEnv
  , parseShardConfig
  , parseShardCount
  , runCommand
  , runExamples
  , runList
  , runTests
  , runUpdates
  , selectShard
  , testTree
  , withSetupDatabase
  )
where

import PGT.Prelude
import PGT.Selector
import PGT.Shard
import PGT.Test
import System.Posix.Types (ProcessID)
import UnliftIO.Exception (bracket_)

import qualified DBT.ClientConfig     as DBT
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text            as Text
import qualified Data.Text.Encoding   as Text
import qualified Data.Text.IO         as Text
import qualified Data.Vector          as Vector
import qualified System.Environment   as System
import qualified System.Exit          as System
import qualified System.Path          as Path
import qualified System.Posix.Process as Process
import qualified System.Process.Typed as Process
import qualified Test.Tasty           as Tasty
import qualified Test.Tasty.MGolden   as Tasty.MGolden
import qualified Test.Tasty.Options   as Tasty
import qualified Test.Tasty.Runners   as Tasty.Runners

data Environment = Environment
  { pgtConfig :: Config
  , pgtPid    :: ProcessID
  }

type PostProcess = Text -> Text

data Config = Config
  { clientConfig :: DBT.ClientConfig
  , setupFile    :: Maybe Path.AbsRelFile
  }
  deriving stock Show

data Command = Command (Tests -> MIO Environment System.ExitCode) ShardCount ShardIndex (Vector Selector)

runCommand :: forall m . MonadUnliftIO m => Command -> Config -> m System.ExitCode
runCommand (Command action shardCount shardIndex selectors) pgtConfig = do
  pgtPid <- liftIO Process.getProcessID
  runMIO Environment{..} $ do
    shardConfig <- either throwString pure $ parseShardConfig shardCount shardIndex
    action . selectShard shardConfig =<< expand selectors

runList :: Tests -> MIO Environment System.ExitCode
runList tests = traverse_ printTest tests $> System.ExitSuccess
  where
    printTest :: Test -> MIO Environment ()
    printTest Test{..} = liftIO . Text.putStrLn . convertText $ Path.toString path

runExamples :: Vector Test -> MIO Environment System.ExitCode
runExamples tests = do
  withSetupDatabase $ \setupDatabaseName -> do
    traverse_ (runTestSession setupDatabaseName Process.runProcess_) tests
    pure System.ExitSuccess

runTasty :: PostProcess -> Tasty.OptionSet -> Tests -> MIO Environment System.ExitCode
runTasty postProcess tastyOptions tests = do
  withSetupDatabase $ \setupDatabaseName -> do
    liftIO Tasty.Runners.installSignalHandlers
    maybe (liftIO failIngredients) (liftIO . run)
      . Tasty.Runners.tryIngredients Tasty.defaultIngredients tastyOptions
      =<< testTree setupDatabaseName postProcess tests
  where
    failIngredients :: IO a
    failIngredients = fail "Internal failure running ingredients"

    run :: IO Bool -> IO System.ExitCode
    run action = do
      ok <- action
      pure $ if ok
        then System.ExitSuccess
        else System.ExitFailure 1

runTests :: PostProcess -> Tests -> MIO Environment System.ExitCode
runTests postProcess = runTasty postProcess mempty

runUpdates :: PostProcess -> Tests -> MIO Environment System.ExitCode
runUpdates postProcess = runTasty postProcess (Tasty.singleOption Tasty.MGolden.UpdateExpected)

testTree :: DBT.DatabaseName -> PostProcess -> Tests -> MIO Environment Tasty.TestTree
testTree setupDatabaseName postProcess tests = Tasty.testGroup "pgt" <$> traverse mkGolden (Vector.toList tests)
  where
    mkGolden :: Test -> MIO Environment Tasty.TestTree
    mkGolden test@Test{..} = do
      env <- ask

      pure
        $ Tasty.MGolden.goldenTest (Path.toString path) (Path.toString $ expectedFileName test)
        $ runMIO env $ captureTest setupDatabaseName postProcess test

expectedFileName :: Test -> Path.RelFile
expectedFileName Test{..} = Path.addExtension path ".expected"

captureTest
  :: DBT.DatabaseName
  -> PostProcess
  -> Test
  -> MIO Environment Text
captureTest setupDatabaseName postProcess
  = runTestSession setupDatabaseName
  $ (postProcess . stripTrailing . Text.decodeUtf8 . LBS.toStrict <$>)
  . Process.readProcessInterleaved_
  where
    stripTrailing
      = (`Text.snoc` '\n')
      . Text.stripEnd
      . Text.unlines
      . (Text.stripEnd <$>)
      . Text.lines

runTestSession
  :: forall a . DBT.DatabaseName
  -> (Process.ProcessConfig () () () -> MIO Environment a)
  -> Test
  -> MIO Environment a
runTestSession setupDatabaseName runProcess test@Test{..} = withTestDatabase setupDatabaseName test runTest
  where
    runTest :: DBT.ClientConfig -> MIO Environment a
    runTest psqlConfig = do
      env  <- DBT.getEnv psqlConfig
      body <- liftIO $ LBS.readFile (Path.toString path)

      runEnvProcess
        (runProcess . Process.setStdin (Process.byteStringInput body))
        env
        "psql"
        [ "--echo-queries"
        , "--no-password"
        , "--no-psqlrc"
        , "--no-readline"
        , "--set", "ON_ERROR_STOP=1"
        ]

withSetupDatabase :: forall a . (DBT.DatabaseName -> MIO Environment a) -> MIO Environment a
withSetupDatabase action = do
  Config{..} <- asks (.pgtConfig)

  maybe (action clientConfig.databaseName) setupDatabase setupFile
  where
    setupDatabase :: Path.AbsRelFile -> MIO Environment a
    setupDatabase setupFile = do
      clientConfig <- asks (.pgtConfig.clientConfig)
      pid          <- asks (.pgtPid)

      let setupDatabaseName = DBT.DatabaseName
                            $ Text.intercalate "_"
                            [ convert clientConfig.databaseName
                            , convert $ show pid
                            , "setup"
                            ]

      withDatabaseFromTemplate setupDatabaseName clientConfig.databaseName $ do
        env <- DBT.getEnv clientConfig{DBT.databaseName = setupDatabaseName}

        runEnvProcess
          Process.runProcess_
          env
          "psql"
          [ "--file", Path.toString setupFile
          , "--no-password"
          , "--no-psqlrc"
          , "--no-readline"
          , "--quiet"
          , "--set", "ON_ERROR_STOP=1"
          ]

        action setupDatabaseName


withTestDatabase
  :: DBT.DatabaseName
  -> Test
  -> (DBT.ClientConfig -> MIO Environment a)
  -> MIO Environment a
withTestDatabase mainDatabaseName Test{..} action = do
  Config{..} <- asks (.pgtConfig)
  pid        <- asks (.pgtPid)

  let testDatabaseName = DBT.DatabaseName
                       $ Text.intercalate "_"
                       [ convert mainDatabaseName
                       , convert $ show pid
                       , convert $ show id
                       ]

  withDatabaseFromTemplate testDatabaseName mainDatabaseName
    (action clientConfig{DBT.databaseName = testDatabaseName})

withDatabaseFromTemplate :: DBT.DatabaseName -> DBT.DatabaseName -> MIO Environment a -> MIO Environment a
withDatabaseFromTemplate databaseName templateDatabaseName action =
  withDBEnv =<< DBT.getEnv =<< asks (.pgtConfig.clientConfig)
  where
    withDBEnv env =
      bracket_
        (runEnvProcess
          Process.runProcess_
          env
          "createdb"
          [ "--template", convertText templateDatabaseName
          , "--"
          , convertText databaseName
          ]
        )
        (runEnvProcess Process.runProcess_ env "dropdb" ["--", convertText databaseName])
        action

runEnvProcess
  :: (Process.ProcessConfig () () () -> m a)
  -> [(String, String)]
  -> String
  -> [String]
  -> m a
runEnvProcess runProcess env name arguments
  = runProcess
  . Process.setEnv env
  $ Process.proc name arguments

fromEnv :: forall m . MonadIO m => m Config
fromEnv = do
  databaseName <- DBT.DatabaseName <$> readEnv "PGDATABASE"
  hostName     <- DBT.HostName     <$> readEnv "PGHOST"
  userName     <- DBT.UserName     <$> readEnv "PGUSER"

  sslMode      <- pure . DBT.SSLMode      <$> readEnv "PGSSLMODE"
  sslRootCert  <- pure . DBT.SSLRootCert  <$> readEnv "PGSSLROOTCERT"

  hostPort     <- pure <$> (DBT.parseHostPort =<< readEnv "PGPORT")
  setupFile    <- fmap (Path.file . convert) <$> liftIO (System.lookupEnv "PGTSETUP")

  pure $ Config{clientConfig = DBT.ClientConfig{password = empty, ..}, ..}
  where
    readEnv :: String -> m Text
    readEnv = (convertText <$>) . liftIO . System.getEnv
