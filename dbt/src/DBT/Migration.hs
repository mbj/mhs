{-# LANGUAGE QuasiQuotes #-}

module DBT.Migration
  ( Config(..)
  , Env
  , RunPGDump
  , WithClientConfig
  , WithDatabaseInstance
  , applyPending
  , applyPendingDumpSchema
  , create
  , dryApply
  , dumpSchema
  , loadSchema
  , printStatus
  , setup
  )
where

import Control.Bool (whenM)
import Control.Monad.Reader (asks)
import DBT.Prelude
import Data.Bifunctor (bimap)
import GHC.Enum (succ)
import Prelude (error)
import System.Path ((<.>), (</>))
import Text.Read (readMaybe)
import UnliftIO.Exception (throwIO, throwString)

import qualified Crypto.Hash                as Hash
import qualified DBT.ClientConfig           as DBT
import qualified DBT.Connection             as DBT
import qualified Data.ByteArray             as BA
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.Foldable              as Foldable
import qualified Data.List                  as List
import qualified Hasql.Session              as Hasql
import qualified Hasql.TH                   as Hasql
import qualified Hasql.Transaction          as Transaction
import qualified Hasql.Transaction.Sessions as Transaction
import qualified MIO.Log                    as Log
import qualified System.Path                as Path
import qualified System.Path.Directory      as Path

type Digest = Hash.Digest Hash.SHA3_256

type Env env databaseInstance = (HasField "dbtMigrationConfig" env (Config env databaseInstance), Log.Env env)

type RunPGDump            env databaseInstance =
  databaseInstance -> [Text] -> MIO env LBS.ByteString
type WithClientConfig     env databaseInstance =
  forall a . databaseInstance -> (DBT.ClientConfig -> MIO env a) -> MIO env a
type WithDatabaseInstance env databaseInstance =
  forall a . (databaseInstance -> MIO env a) -> MIO env a

data AppliedMigration = AppliedMigration
  { digest :: Digest
  , index  :: Natural
  }
  deriving stock Show

data MigrationFile = MigrationFile
  { digest :: Digest
  , index  :: Natural
  , path   :: Path.RelFile
  , sql    :: BS.ByteString
  }

data Config env databaseInstance = Config
  { migrationDirectory   :: Path.AbsRelDir
  , runPGDump            :: RunPGDump env databaseInstance
  , schemaFile           :: Path.AbsRelFile
  , withClientConfig     :: WithClientConfig env databaseInstance
  , withDatabaseInstance :: WithDatabaseInstance env databaseInstance
  }

dryApply :: Env env databaseInstance => MIO env ()
dryApply = do
  Config{..} <- readConfig
  withDatabaseInstance $ \databaseInstance -> do
    files <- readPendingMigrations databaseInstance

    if Foldable.null files
      then Log.info "No pending migrations to apply"
      else Foldable.traverse_ printMigrationFile files

printStatus :: Env env databaseInstance => MIO env ()
printStatus = do
  Config{..} <- readConfig
  withDatabaseInstance $ \databaseInstance -> do
    applied <- readAppliedMigrations databaseInstance

    Log.info $ "Applied migrations: " <> convert (show (Foldable.length applied))

    Foldable.traverse_ (Log.info . convert . show) applied

    Foldable.traverse_ printMigrationFile . pendingMigrations applied =<< readMigrationFiles

dumpSchema :: Env env databaseInstance => MIO env ()
dumpSchema = do
  Config{..} <- readConfig
  withDatabaseInstance dumpSchema'

dumpSchema' :: Env env databaseInstance => databaseInstance -> MIO env ()
dumpSchema' databaseInstance = do
  Config{..} <- readConfig
  Log.info $ "Writing schema to " <> convert (Path.toString schemaFile)
  schema     <- runPGDump databaseInstance ["--no-comments", "--schema-only"]
  migrations <- runPGDump databaseInstance ["--data-only", "--inserts", "--table=schema_migrations"]
  liftIO $ LBS.writeFile (Path.toString schemaFile) $ schema <> migrations

loadSchema :: Env env databaseInstance => MIO env ()
loadSchema = do
  Config{..} <- readConfig
  let schemaFileString = Path.toString schemaFile
  withDatabaseInstance $ \databaseInstance -> do
    Log.info $ "Loading schema from " <> convert schemaFileString
    runSession databaseInstance $ Transaction.transaction
      Transaction.Serializable
      Transaction.Write . Transaction.sql =<< liftIO (BS.readFile schemaFileString)

applyPending :: Env env databaseInstance => MIO env ()
applyPending = do
  Config{..} <- readConfig

  withDatabaseInstance applyPending'

applyPendingDumpSchema :: Env env databaseInstance => MIO env ()
applyPendingDumpSchema = do
  Config{..} <- readConfig

  withDatabaseInstance $ \databaseInstance -> do
    applyPending' databaseInstance
    dumpSchema' databaseInstance

applyPending' :: Env env databaseInstance => databaseInstance -> MIO env ()
applyPending' databaseInstance = do
  migrations <- readPendingMigrations databaseInstance
  Log.info $ "Applying " <> convert (show $ Foldable.length migrations) <> " pending migrations"
  Foldable.traverse_ (applyMigration databaseInstance) migrations

create :: Env env databaseInstance => MIO env ()
create = do
  Config{..} <- readConfig

  withDatabaseInstance $ \databaseInstance -> do
    applied       <- max . fmap (.index) <$> readAppliedMigrations databaseInstance
    files         <- max . fmap (.index) <$> readMigrationFiles

    let index = succ $ max ([applied, files] :: [Natural])
        file  = Path.toString $ migrationDirectory </> Path.relFile (show index) <.> ".sql"

    Log.info $ "Creating new migration file: " <> convert file

    liftIO $ do
      Path.createDirectoryIfMissing True migrationDirectory
      BS.writeFile file ""

  where
    max :: (Num a, Ord a) => [a] -> a
    max xs = if Foldable.null xs then 0 else Foldable.maximum xs

setup :: Env env databaseInstance => MIO env ()
setup = do
  Config{..} <- readConfig

  withDatabaseInstance $ \databaseInstance -> do
    runSession databaseInstance $ Hasql.sql
      [Hasql.uncheckedSql|
        CREATE TABLE IF NOT EXISTS
          schema_migrations
          ( index           int8  NOT NULL CHECK (index > 0)
          , digest_sha3_256 bytea NOT NULL CHECK (octet_length(digest_sha3_256) = 32)
          , PRIMARY KEY (index)
          )
      |]
    dumpSchema' databaseInstance

printMigrationFile :: Env env databaseInstance => MigrationFile -> MIO env ()
printMigrationFile MigrationFile{..} = Log.info . convert $ Path.toString path

applyMigration :: Env env databaseInstance => databaseInstance -> MigrationFile -> MIO env ()
applyMigration databaseInstance MigrationFile{..} = do
  Log.info $ "Applying migration: " <> convert (Path.toString path)
  runSession databaseInstance . Transaction.transaction Transaction.Serializable Transaction.Write $ do
    Transaction.sql sql

    Transaction.statement (BS.pack $ BA.unpack digest, convertImpure index)
      [Hasql.resultlessStatement|
        INSERT INTO
          schema_migrations
          (digest_sha3_256, index)
        VALUES
          ($1 :: bytea, $2 :: int8)
      |]

  Log.info "Success"

readAppliedMigrations :: Env env databaseInstance => databaseInstance -> MIO env [AppliedMigration]
readAppliedMigrations databaseInstance =
  runSession databaseInstance $ do
    rows <- Foldable.toList <$> Hasql.statement ()
      [Hasql.vectorStatement|
        SELECT
          digest_sha3_256 :: bytea
        , index           :: int8
        FROM
          schema_migrations
        ORDER BY
          index
      |]
    pure $ uncurry AppliedMigration . bimap fromByteString convertImpure <$> rows
  where
    fromByteString :: BS.ByteString -> Hash.Digest Hash.SHA3_256
    fromByteString
      = maybe (error "Failed to decode sha3_256 from DB") identity
      . Hash.digestFromByteString

readMigrationFiles
  :: forall env databaseInstance . Env env databaseInstance
  => MIO env [MigrationFile]
readMigrationFiles = do
  migrationDirectory <- readMigrationDirectory
  whenM (liftIO (Path.doesDirectoryExist migrationDirectory))
    $ Foldable.foldMap load =<< liftIO (Path.filesInDir migrationDirectory)
  where
    load :: Path.RelFile ->  MIO env [MigrationFile]
    load file =
      whenM (pure $ isMigrationFile file)
        fmap pure . readMigration file =<< parseIndex file

    isMigrationFile :: Path.RelFile -> Bool
    isMigrationFile = (== ".sql") . Path.takeExtension

    parseIndex :: Path.RelFile -> MIO env Natural
    parseIndex file =
      maybe
        (throwString $ "Invalid migration file name: " <> Path.toString file)
        pure
        (readMaybe . Path.toString $ Path.dropExtension file)

    readMigration :: Path.RelFile -> Natural -> MIO env MigrationFile
    readMigration path index = do
      migrationDirectory <- readMigrationDirectory
      sql <- liftIO $ BS.readFile . Path.toString $ migrationDirectory </> path
      pure MigrationFile
        { digest = Hash.hash sql
        , ..
        }

readMigrationDirectory :: Env env databaseInstance => MIO env Path.AbsRelDir
readMigrationDirectory = (.migrationDirectory) <$> readConfig

readPendingMigrations :: Env env databaseInstance => databaseInstance -> MIO env [MigrationFile]
readPendingMigrations databaseInstance =
  pendingMigrations <$> readAppliedMigrations databaseInstance <*> readMigrationFiles

pendingMigrations :: [AppliedMigration] -> [MigrationFile] -> [MigrationFile]
pendingMigrations applied = List.sortOn (.index) . Foldable.foldMap test
  where
    test :: MigrationFile -> [MigrationFile]
    test file = guard' (not $ isApplied file.index) file

    isApplied :: Natural -> Bool
    isApplied fileIndex' = Foldable.any ((==) fileIndex' . (.index)) applied

runSession :: Env env databaseInstance => databaseInstance -> Hasql.Session a -> MIO env a
runSession databaseInstance session = either throwIO pure =<< runSessionEither
  where
    runSessionEither = do
      Config{..} <- readConfig

      withClientConfig databaseInstance $ \clientConfig ->
        DBT.withConnection clientConfig $ liftIO . Hasql.run session

readConfig :: Env env databaseInstance => MIO env (Config env databaseInstance)
readConfig = asks (.dbtMigrationConfig)
