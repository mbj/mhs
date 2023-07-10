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
  )
where

import PGT.Prelude
import PGT.Selector
import PGT.Shard
import PGT.Test
import System.Posix.Types (ProcessID)
import UnliftIO.Exception (bracket)

import qualified DBT.ClientConfig     as DBT
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text            as Text
import qualified Data.Text.Encoding   as Text
import qualified Data.Text.IO         as Text
import qualified Data.Vector          as Vector
import qualified PGT.Output           as PGT
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

newtype Config = Config
  { clientConfig :: DBT.ClientConfig
  }
  deriving stock Show

data Command = Command (Tests -> MIO Environment System.ExitCode) ShardCount ShardIndex [Selector]

runCommand :: forall m . MonadUnliftIO m => Command -> Config -> m System.ExitCode
runCommand (Command action shardCount shardIndex selectors) pgtConfig = do
  pgtPid <- liftIO Process.getProcessID
  runMIO Environment{..} $ do
    shardConfig <- either throwString pure $ parseShardConfig shardCount shardIndex
    action . selectShard shardConfig . Vector.fromList =<< expand selectors

runList :: Tests -> MIO Environment System.ExitCode
runList tests = traverse_ printTest tests $> System.ExitSuccess
  where
    printTest :: Test -> MIO Environment ()
    printTest Test{..} = liftIO . Text.putStrLn . convertText $ Path.toString path

runExamples :: Vector Test -> MIO Environment System.ExitCode
runExamples tests = do
  traverse_ (runTestSession Process.runProcess_) tests
  pure System.ExitSuccess

runTasty :: Tasty.OptionSet -> [Test] -> MIO Environment System.ExitCode
runTasty tastyOptions tests = do
  liftIO Tasty.Runners.installSignalHandlers
  maybe (liftIO failIngredients) (liftIO . run)
    . Tasty.Runners.tryIngredients Tasty.defaultIngredients tastyOptions
    =<< testTree PGT.impureParse tests
  where
    failIngredients :: IO a
    failIngredients = fail "Internal failure running ingredients"

    run :: IO Bool -> IO System.ExitCode
    run action = do
      ok <- action
      pure $ if ok
        then System.ExitSuccess
        else System.ExitFailure 1

runTests :: Tests -> MIO Environment System.ExitCode
runTests = runTasty mempty . Vector.toList

runUpdates :: Tests -> MIO Environment System.ExitCode
runUpdates
  = runTasty (Tasty.singleOption Tasty.MGolden.UpdateExpected)
  . Vector.toList

testTree :: PostProcess -> [Test] -> MIO Environment Tasty.TestTree
testTree postProcess tests = Tasty.testGroup "pgt" <$> traverse (mkGolden postProcess) tests

mkGolden :: PostProcess -> Test -> MIO Environment Tasty.TestTree
mkGolden postProcess test@Test{..} = do
  env <- ask

  pure $ Tasty.MGolden.goldenTest (Path.toString path) (Path.toString $ expectedFileName test)
    $ runMIO env $ captureTest postProcess test

expectedFileName :: Test -> Path.RelFile
expectedFileName Test{..} = Path.addExtension path ".expected"

captureTest
  :: PostProcess
  -> Test
  -> MIO Environment Text
captureTest postProcess
  = runTestSession
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
  :: forall a . (Process.ProcessConfig () () () -> MIO Environment a)
  -> Test
  -> MIO Environment a
runTestSession runProcess test@Test{..} =
  withTestDatabase test runTest
  where
    runTest :: DBT.ClientConfig -> MIO Environment a
    runTest psqlConfig = do
      env  <- DBT.getEnv psqlConfig
      body <- LBS.fromStrict . Text.encodeUtf8 <$> readFile path

      runProcess
        . Process.setEnv env
        . Process.setStdin (Process.byteStringInput body)
        $ Process.proc "psql" arguments

    arguments :: [String]
    arguments =
      [ "--echo-queries"
      , "--no-password"
      , "--no-psqlrc"
      , "--no-readline"
      , "--set"
      , "ON_ERROR_STOP=1"
      ]

withTestDatabase
  :: Test
  -> (DBT.ClientConfig -> MIO Environment a)
  -> MIO Environment a
withTestDatabase test action = do
  Config{..} <- asks (.pgtConfig)
  bracket
    (createTestDatabase test)
    dropDatabase
    (\testDatabase -> action clientConfig{DBT.databaseName = testDatabase})

createTestDatabase :: Test -> MIO Environment DBT.DatabaseName
createTestDatabase Test{..} = do
  Config{..} <- asks (.pgtConfig)
  pid        <- asks (.pgtPid)

  let mainDatabaseName = clientConfig.databaseName
      testDatabaseName = DBT.DatabaseName
                       $ Text.intercalate "_"
                       [ convert mainDatabaseName
                       , convert $ show pid
                       , convert $ show id
                       ]

  env <- DBT.getEnv clientConfig

  Process.runProcess_
    . Process.setEnv env
    $ Process.proc "createdb"
    [ "--template", convertText mainDatabaseName
    , "--"
    , convertText testDatabaseName
    ]

  pure testDatabaseName

dropDatabase :: DBT.DatabaseName -> MIO Environment ()
dropDatabase databaseName = do
  env <- DBT.getEnv =<< asks (.pgtConfig.clientConfig)
  Process.runProcess_ . Process.setEnv env $ Process.proc "dropdb" ["--", convertText databaseName]

fromEnv :: forall m . MonadIO m => m Config
fromEnv = do
  databaseName <- DBT.DatabaseName <$> lookup "PGDATABASE"
  hostName     <- DBT.HostName     <$> lookup "PGHOST"
  userName     <- DBT.UserName     <$> lookup "PGUSER"

  sslMode      <- pure . DBT.SSLMode      <$> lookup "PGSSLMODE"
  sslRootCert  <- pure . DBT.SSLRootCert  <$> lookup "PGSSLROOTCERT"

  hostPort     <- pure <$> (DBT.parseHostPort =<< lookup "PGPORT")

  pure $ Config{clientConfig = DBT.ClientConfig{password = empty, ..}}
  where
    lookup :: String -> m Text
    lookup = (convertText <$>) . liftIO . System.getEnv

readFile :: MonadIO m => Path.RelFile -> m Text
readFile = liftIO . Text.readFile . Path.toString
