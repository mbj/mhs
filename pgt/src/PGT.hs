module PGT
  ( Config(..)
  , ShardConfig
  , ShardCount
  , ShardIndex(..)
  , Test(..)
  , Tests
  , configure
  , defaultShardCount
  , defaultShardIndex
  , fromEnv
  , parseShardConfig
  , parseShardCount
  , runExamples
  , runList
  , runTests
  , runUpdates
  , selectShard
  , testTree
  )
where

import Data.Vector (Vector)
import PGT.Prelude
import PGT.Shard
import System.Posix.Types (ProcessID)
import UnliftIO.Exception (bracket)

import qualified DBT.ClientConfig           as DBT
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.Text                  as Text
import qualified Data.Text.Encoding         as Text
import qualified Data.Text.IO               as Text
import qualified Data.Vector                as Vector
import qualified PGT.Output                 as PGT
import qualified System.Environment         as System
import qualified System.Exit                as System
import qualified System.Path                as Path
import qualified System.Posix.Process       as Process
import qualified System.Process.Typed       as Process
import qualified Test.Tasty                 as Tasty
import qualified Test.Tasty.MGolden         as Tasty.MGolden
import qualified Test.Tasty.Options         as Tasty
import qualified Test.Tasty.Runners         as Tasty.Runners

data Test = Test
  { id   :: Natural
  , path :: Path.RelFile
  }
  deriving stock (Eq, Ord)

type Tests = Vector Test

data Config = Config
  { pid       :: ProcessID
  , psqlAdmin :: DBT.ClientConfig
  , psqlUser  :: DBT.ClientConfig
  }
  deriving stock Show

runList :: forall m . (MonadIO m) => Config -> Tests -> m System.ExitCode
runList _config tests = traverse_ printTest tests $> System.ExitSuccess
  where
    printTest :: Test -> m ()
    printTest Test{..} = liftIO . Text.putStrLn . convertText $ Path.toString path

runExamples :: forall f m . (Foldable f, MonadUnliftIO m) => Config -> f Test -> m System.ExitCode
runExamples config tests = do
  traverse_ (runTestSession config Process.runProcess_) tests
  pure System.ExitSuccess

type PostProcess = Text -> Text

runTasty :: MonadIO m => Tasty.OptionSet -> Config -> [Test] -> m System.ExitCode
runTasty tastyOptions config tests = liftIO $ do
  Tasty.Runners.installSignalHandlers
  maybe failIngredients run
    . Tasty.Runners.tryIngredients Tasty.defaultIngredients tastyOptions
    $ testTree PGT.impureParse config tests
  where
    failIngredients :: IO a
    failIngredients = fail "Internal failure running ingredients"

    run :: IO Bool -> IO System.ExitCode
    run action = do
      ok <- action
      pure $ if ok
        then System.ExitSuccess
        else System.ExitFailure 1

runTests :: MonadIO m => Config -> Tests -> m System.ExitCode
runTests config = runTasty mempty config . Vector.toList

runUpdates :: MonadIO m => Config -> Tests -> m System.ExitCode
runUpdates config
  = runTasty (Tasty.singleOption Tasty.MGolden.UpdateExpected) config
  . Vector.toList

testTree :: PostProcess -> Config -> [Test] -> Tasty.TestTree
testTree postProcess config tests = Tasty.testGroup "pgt" $ mkGolden postProcess config <$> tests

mkGolden :: PostProcess -> Config -> Test -> Tasty.TestTree
mkGolden postProcess config test@Test{..}
  = Tasty.MGolden.goldenTest
     (Path.toString path)
     (Path.toString $ expectedFileName test)
  $ captureTest postProcess config test

expectedFileName :: Test -> Path.RelFile
expectedFileName Test{..} = Path.addExtension path ".expected"

captureTest
  :: forall m . MonadUnliftIO m
  => PostProcess
  -> Config
  -> Test
  -> m Text
captureTest postProcess config
  = runTestSession config
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
  :: forall a m . MonadUnliftIO m
  => Config
  -> (Process.ProcessConfig () () () -> m a)
  -> Test
  -> m a
runTestSession config runProcess test@Test{..} =
  withTestDatabase config test runSession
  where
    runSession :: DBT.ClientConfig -> m a
    runSession psqlConfig = do
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

withTestDatabase :: MonadUnliftIO m => Config -> Test -> (DBT.ClientConfig -> m a) -> m a
withTestDatabase config@Config{..} test action =
  bracket
    (createTestDatabase config test)
    (dropDatabase psqlAdmin)
    (\testDatabase -> action $ psqlUser { DBT.databaseName = testDatabase })

createTestDatabase :: MonadIO m => Config -> Test -> m DBT.DatabaseName
createTestDatabase Config{..} Test{..} = do
  env <- DBT.getEnv psqlAdmin
  Process.runProcess_ $ Process.setEnv env command
  pure testDatabase
  where
    masterDatabase = psqlAdmin.databaseName

    command = Process.proc
      "createdb"
      [ "--template"
      , convertText masterDatabase
      , "--"
      , convertText testDatabase
      ]

    testDatabase =
      DBT.DatabaseName $
        Text.intercalate
          "_"
          [ convertText masterDatabase
          , convertText $ show pid
          , convertText $ show id
          ]

dropDatabase :: MonadIO m => DBT.ClientConfig -> DBT.DatabaseName -> m ()
dropDatabase config databaseName = do
  env <- DBT.getEnv config
  Process.runProcess_ . Process.setEnv env $ Process.proc "dropdb" ["--", convertText databaseName]

configure
  :: MonadIO m
  => DBT.ClientConfig
  -> Maybe DBT.ClientConfig
  -> m Config
configure psqlAdmin psqlUser = do
  pid          <- liftIO Process.getProcessID

  pure $ Config
    { psqlUser = fromMaybe psqlAdmin psqlUser
    , ..
    }

fromEnv :: forall m . MonadIO m => m Config
fromEnv = do
  databaseName <- DBT.DatabaseName <$> lookup "PGDATABASE"
  hostName     <- DBT.HostName     <$> lookup "PGHOST"
  pgtUser      <- DBT.UserName     <$> lookup "PGTUSER"
  userName     <- DBT.UserName     <$> lookup "PGUSER"

  sslMode      <- pure . DBT.SSLMode      <$> lookup "PGSSLMODE"
  sslRootCert  <- pure . DBT.SSLRootCert  <$> lookup "PGSSLROOTCERT"

  hostPort     <- pure <$> (DBT.parseHostPort =<< lookup "PGPORT")

  let psqlAdmin = DBT.ClientConfig{password = empty, ..}
      psqlUser  = psqlAdmin { DBT.userName = pgtUser }

  configure psqlAdmin $ pure psqlUser
  where
    lookup :: String -> m Text
    lookup = (convertText <$>) . liftIO . System.getEnv

readFile :: MonadIO m => Path.RelFile -> m Text
readFile = liftIO . Text.readFile . Path.toString
