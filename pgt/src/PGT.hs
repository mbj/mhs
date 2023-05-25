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
import Prelude ((-), div, succ)
import System.Posix.Types (ProcessID)
import UnliftIO.Exception (bracket)

import qualified DBT.Postgresql             as Postgresql
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.Foldable              as Foldable
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

newtype ShardCount = ShardCount Natural
  deriving stock (Eq, Show)

newtype ShardIndex = ShardIndex Natural
  deriving stock (Eq, Show)

defaultShardCount :: ShardCount
defaultShardCount = ShardCount 1

defaultShardIndex :: ShardIndex
defaultShardIndex = ShardIndex 0

parseShardCount :: Natural -> Either String ShardCount
parseShardCount = \case
  0 -> Left "Shard count cannot be 0"
  n -> pure $ ShardCount n

parseShardConfig :: ShardCount -> ShardIndex -> Either String ShardConfig
parseShardConfig count@(ShardCount countValue) index@(ShardIndex indexValue) =
  if indexValue >= countValue
    then Left "Shard index ouside of shard count"
    else pure ShardConfig{..}

selectShard :: ShardConfig -> Vector a -> Vector a
selectShard ShardConfig{count=(ShardCount count), index=(ShardIndex index)} items =
    Vector.slice (convertImpure startIndex) (convertImpure effectiveChunkSize) items
  where
    nominalChunkSize :: Natural
    nominalChunkSize = length `div` count

    startIndex :: Natural
    startIndex = index * nominalChunkSize

    effectiveChunkSize :: Natural
    effectiveChunkSize =
      if succ index == count
        then length - startIndex
        else nominalChunkSize

    length :: Natural
    length = convertImpure $ Vector.length items

data ShardConfig = ShardConfig
  { count :: ShardCount
  , index :: ShardIndex
  }
  deriving stock (Eq, Show)

data Test = Test
  { id   :: Natural
  , path :: Path.RelFile
  }
  deriving stock (Eq, Ord)

type Tests = Vector Test

data Config = Config
  { pid       :: ProcessID
  , psqlAdmin :: Postgresql.ClientConfig
  , psqlUser  :: Postgresql.ClientConfig
  }
  deriving stock Show

runList :: forall m . (MonadIO m) => Config -> Tests -> m ()
runList _config = traverse_ printTest
  where
    printTest :: Test -> m ()
    printTest Test{..} = liftIO . Text.putStrLn . convertText $ Path.toString path

runExamples :: forall f m . (Foldable f, MonadUnliftIO m) => Config -> f Test -> m ()
runExamples config = Foldable.mapM_ $ runTestSession config Process.runProcess_

type PostProcess = Text -> Text

runTasty :: MonadIO m => Tasty.OptionSet -> PostProcess -> Config -> [Test] -> m ()
runTasty tastyOptions postProcess config tests = liftIO $ do
  Tasty.Runners.installSignalHandlers
  maybe failIngredients run
    . Tasty.Runners.tryIngredients Tasty.defaultIngredients tastyOptions
    $ testTree postProcess config tests
  where
    failIngredients :: IO a
    failIngredients = fail "Internal failure running ingredients"

    run :: IO Bool -> IO ()
    run action = do
      ok <- action
      if ok
        then System.exitSuccess
        else System.exitFailure

runTests :: MonadIO m => Config -> Tests -> m ()
runTests config = runTasty mempty PGT.impureParse config . Vector.toList

runUpdates :: MonadIO m => Config -> Tests -> m ()
runUpdates = runUpdates' PGT.impureParse

runUpdates' :: MonadIO m => PostProcess -> Config -> Tests -> m ()
runUpdates' postProcess config
  = runTasty (Tasty.singleOption Tasty.MGolden.UpdateExpected) postProcess config
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
    runSession :: Postgresql.ClientConfig -> m a
    runSession psqlConfig = do
      env  <- Postgresql.getEnv psqlConfig
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

withTestDatabase :: MonadUnliftIO m => Config -> Test -> (Postgresql.ClientConfig -> m a) -> m a
withTestDatabase config@Config{..} test action =
  bracket
    (createTestDatabase config test)
    (dropDatabase psqlAdmin)
    (\testDatabase -> action $ psqlUser { Postgresql.databaseName = testDatabase })

createTestDatabase :: MonadIO m => Config -> Test -> m Postgresql.DatabaseName
createTestDatabase Config{..} Test{..} = do
  env <- Postgresql.getEnv psqlAdmin
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
      Postgresql.DatabaseName $
        Text.intercalate
          "_"
          [ convertText masterDatabase
          , convertText $ show pid
          , convertText $ show id
          ]

dropDatabase :: MonadIO m => Postgresql.ClientConfig -> Postgresql.DatabaseName -> m ()
dropDatabase config databaseName = do
  env <- Postgresql.getEnv config
  Process.runProcess_ . Process.setEnv env $ Process.proc "dropdb" ["--", convertText databaseName]

configure
  :: MonadIO m
  => Postgresql.ClientConfig
  -> Maybe Postgresql.ClientConfig
  -> m Config
configure psqlAdmin psqlUser = do
  pid          <- liftIO Process.getProcessID

  pure $ Config
    { psqlUser = fromMaybe psqlAdmin psqlUser
    , ..
    }

fromEnv :: forall m . MonadIO m => m Config
fromEnv = do
  databaseName <- Postgresql.DatabaseName <$> lookup "PGDATABASE"
  hostName     <- Postgresql.HostName     <$> lookup "PGHOST"
  pgtUser      <- Postgresql.UserName     <$> lookup "PGTUSER"
  userName     <- Postgresql.UserName     <$> lookup "PGUSER"

  sslMode      <- pure . Postgresql.SSLMode      <$> lookup "PGSSLMODE"
  sslRootCert  <- pure . Postgresql.SSLRootCert  <$> lookup "PGSSLROOTCERT"

  hostPort     <- pure <$> (Postgresql.parseHostPort =<< lookup "PGPORT")

  let psqlAdmin = Postgresql.ClientConfig{password = empty, ..}
      psqlUser  = psqlAdmin { Postgresql.userName = pgtUser }

  configure psqlAdmin $ pure psqlUser
  where
    lookup :: String -> m Text
    lookup = (convertText <$>) . liftIO . System.getEnv

readFile :: MonadIO m => Path.RelFile -> m Text
readFile = liftIO . Text.readFile . Path.toString
