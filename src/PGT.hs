module PGT
  ( Config
  , PSQLConfig
  , Test(..)
  , fromEnv
  , runExamples
  , runList
  , runTests
  , runUpdates
  )
where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Bifunctor (second)
import Data.String (String)
import Numeric.Natural (Natural)
import PGT.Prelude
import System.Posix.Types (ProcessID)
import UnliftIO.Exception (bracket)

import qualified Data.ByteString.Lazy       as LBS
import qualified Data.Foldable              as Foldable
import qualified Data.Text                  as Text
import qualified Data.Text.Encoding         as Text
import qualified Data.Text.IO               as Text
import qualified System.Environment         as Environment
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

data Config = Config
  { pid        :: ProcessID
  , psqlAdmin  :: PSQLConfig
  , psqlUser   :: PSQLConfig
  }

data PSQLConfig = PSQLConfig
  { database    :: Text
  , host        :: Text
  , path        :: Text
  , port        :: Text
  , sslmode     :: Text
  , sslrootcert :: Text
  , user        :: Text
  }

runList :: forall f m . (Foldable f, MonadIO m) => Config -> f Test -> m ()
runList _config = Foldable.mapM_ printTest
  where
    printTest :: Test -> m ()
    printTest Test{..} = liftIO . Text.putStrLn . convertText $ Path.toString path

runExamples :: forall f m . (Foldable f, MonadUnliftIO m) => Config -> f Test -> m ()
runExamples config = Foldable.mapM_ $ runTestSession config Process.runProcess_

runTasty :: MonadIO m => Tasty.OptionSet -> Config -> [Test] -> m ()
runTasty tastyOptions config tests = liftIO $ do
  Tasty.Runners.installSignalHandlers
  maybe failIngredients run
    $ Tasty.Runners.tryIngredients Tasty.defaultIngredients tastyOptions goldenTests
  where
    goldenTests = Tasty.testGroup "pgt" $ mkGolden config <$> tests

    failIngredients :: IO a
    failIngredients = fail "Internal failure running ingredients"

    run :: IO Bool -> IO ()
    run action = do
      ok <- action
      if ok
        then System.exitSuccess
        else System.exitFailure

runTests :: MonadIO m => Config -> [Test] -> m ()
runTests = runTasty mempty

runUpdates :: MonadIO m => Config -> [Test] -> m ()
runUpdates = runTasty (Tasty.singleOption Tasty.MGolden.UpdateExpected)

mkGolden :: Config -> Test -> Tasty.TestTree
mkGolden config test@Test{..}
  = Tasty.MGolden.goldenTest
     (Path.toString path)
     (Path.toString $ expectedFileName test)
  $ captureTest config test

expectedFileName :: Test -> Path.RelFile
expectedFileName Test{..} = Path.replaceExtension path ".expected"

captureTest :: forall m . MonadUnliftIO m => Config -> Test -> m Text
captureTest config
  = runTestSession config
  $ (stripTrailing . Text.decodeUtf8 . LBS.toStrict <$>)
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
    runSession :: PSQLConfig -> m a
    runSession psqlConfig =
      runProcess . psql psqlConfig =<< LBS.fromStrict . Text.encodeUtf8 <$> readFile path

    psql psqlConfig body
      = pgEnv psqlConfig
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

withTestDatabase :: MonadUnliftIO m => Config -> Test -> (PSQLConfig -> m a) -> m a
withTestDatabase config@Config{..} test action =
  bracket
    (createTestDatabase config test)
    (dropDatabase psqlAdmin)
    (\testDatabase -> action $ psqlUser { database = testDatabase })

createTestDatabase :: MonadIO m => Config -> Test -> m Text
createTestDatabase Config{..} Test{..} = do
  Process.runProcess_ $ pgEnv psqlAdmin command
  pure testDatabase
  where
    masterDatabase = database psqlAdmin

    command = Process.proc
      "createdb"
      [ "--template"
      , convertText masterDatabase
      , "--"
      , convertText testDatabase
      ]

    testDatabase =
      Text.intercalate
        "_"
        [ masterDatabase
        , convertText $ show pid
        , convertText $ show id
        ]

dropDatabase :: MonadIO m => PSQLConfig -> Text -> m ()
dropDatabase config dbName =
  Process.runProcess_ . pgEnv config $ Process.proc "dropdb" ["--", convertText dbName]

pgEnv :: PSQLConfig -> Process.ProcessConfig a b c -> Process.ProcessConfig a b c
pgEnv PSQLConfig{..} = Process.setEnv env
  where
    env :: [(String, String)]
    env = second convertText <$>
      [ ("PATH",          path)
      , ("PGDATABASE",    database)
      , ("PGHOST",        host)
      , ("PGPORT",        port)
      , ("PGSSLMODE",     sslmode)
      , ("PGSSLROOTCERT", sslrootcert)
      , ("PGUSER",        user)
      ]

fromEnv :: forall m . MonadIO m => m Config
fromEnv = do
  database    <- lookup "PGDATABASE"
  host        <- lookup "PGHOST"
  path        <- lookup "PATH"
  pgtUser     <- lookup "PGTUSER"
  pid         <- liftIO Process.getProcessID
  port        <- lookup "PGPORT"
  sslmode     <- lookup "PGSSLMODE"
  sslrootcert <- lookup "PGSSLROOTCERT"
  user        <- lookup "PGUSER"
  let psqlAdmin = PSQLConfig{..}
      psqlUser  = psqlAdmin { user = pgtUser }

  pure Config{..}
  where
    lookup :: String -> m Text
    lookup = (convertText <$>) . liftIO . Environment.getEnv

readFile :: MonadIO m => Path.RelFile -> m Text
readFile = liftIO . Text.readFile . Path.toString
