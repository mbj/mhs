module PGT
  ( Config
  , FirstError(Continue, Stop)
  , Options(..)
  , Output(..)
  , PSQLConfig
  , Test(..)
  , fromEnv
  , runExamples
  , runList
  , runTests
  , runUpdates
  )
where

import Control.Monad (sequence_)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Bifunctor (second)
import Data.String (String)
import Numeric.Natural (Natural)
import PGT.Formatter
import PGT.Prelude
import System.Environment (getEnv)
import System.FilePath (FilePath, (-<.>))
import System.Posix.Process (getProcessID)
import System.Posix.Types (ProcessID)
import UnliftIO.Exception (bracket)

import qualified Data.ByteString.Lazy       as LBS
import qualified Data.Foldable              as Foldable
import qualified Data.Text                  as Text
import qualified Data.Text.Encoding         as Text
import qualified Data.Text.IO               as Text
import qualified System.Process.Typed       as Process
import qualified Test.Hspec                 as Hspec
import qualified Test.Hspec.Core.Formatters as Hspec
import qualified Test.Hspec.Runner          as Hspec

data Test = Test
  { id   :: Natural
  , path :: FilePath
  }
  deriving stock (Eq, Ord)

data Config = Config
  { firstError :: FirstError
  , output     :: Output
  , pid        :: ProcessID
  , psqlAdmin  :: PSQLConfig
  , psqlUser   :: PSQLConfig
  }

data FirstError = Continue | Stop
data Output     = Silent | Verbose

data Options = Options
  { firstError :: FirstError
  , output     :: Output
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
runList config = Foldable.mapM_ printTest
  where
    printTest :: Test -> m ()
    printTest Test{..} = print config $ convertText path

runExamples :: forall f m . (Foldable f, MonadUnliftIO m) => Config -> f Test -> m ()
runExamples config = Foldable.mapM_ $ runTestSession config Process.runProcess_

print :: MonadIO m => Config -> Text -> m ()
print Config{ output = Verbose } = liftIO . Text.putStrLn
print Config{ output = Silent }  = const $ pure ()

runTests :: (Foldable f, Functor f, MonadIO m) => Config -> f Test -> m ()
runTests config@Config{..} tests = liftIO $ Hspec.evaluateSummary =<< Hspec.runSpec spec hspecConfig
  where
    spec = Hspec.describe "pgt" $ sequence_ (makeSpec <$> tests)

    makeSpec test@Test{..} =
      Hspec.specify path $ do
        captured <- captureTest config test
        expected <- Text.readFile $ expectedFileName test
        captured `Hspec.shouldBe` expected

    hspecConfig = Hspec.defaultConfig
      { Hspec.configFastFail  = hspecFastFail firstError
      , Hspec.configFormatter = pure $ hspecFormatter output
      }

    hspecFastFail Continue = False
    hspecFastFail Stop     = True

    hspecFormatter Silent  = Hspec.silent
    hspecFormatter Verbose = Hspec.progress { Hspec.failedFormatter = multilineFailedFormatter }

runUpdates :: forall f m . (Foldable f, MonadIO m) => Config -> f Test -> m ()
runUpdates config = Foldable.mapM_ updateTest
  where
    updateTest :: Test -> m ()
    updateTest test =
      liftIO $ Text.writeFile (expectedFileName test) =<< captureTest config test

expectedFileName :: Test -> FilePath
expectedFileName Test{..} = path -<.> ".expected"

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
      runProcess . psql psqlConfig =<< LBS.fromStrict . Text.encodeUtf8 <$> liftIO (Text.readFile path)

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

fromEnv :: forall m . MonadIO m => Options -> m Config
fromEnv Options{..} = do
  database    <- lookup "PGDATABASE"
  host        <- lookup "PGHOST"
  path        <- lookup "PATH"
  pgtUser     <- lookup "PGTUSER"
  pid         <- liftIO getProcessID
  port        <- lookup "PGPORT"
  sslmode     <- lookup "PGSSLMODE"
  sslrootcert <- lookup "PGSSLROOTCERT"
  user        <- lookup "PGUSER"
  let psqlAdmin = PSQLConfig{..}
      psqlUser  = psqlAdmin { user = pgtUser }

  pure Config{..}
  where
    lookup :: String -> m Text
    lookup = (convertText <$>) . liftIO . getEnv
