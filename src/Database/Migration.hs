{-# LANGUAGE QuasiQuotes #-}

module Database.Migration
  ( apply
  , dryApply
  , dumpSchema
  , loadSchema
  , new
  , schemaFileString
  , setup
  , status
  )
where

import Control.Bool (guard', whenM)
import DBT.Session
import Data.Bifunctor (bimap)
import Data.ByteString (ByteString)
import Database.Migration.Prelude
import GHC.Enum (succ)
import Hasql.TH (resultlessStatement, uncheckedSql, vectorStatement)
import Prelude (error)
import System.Path ((<.>), (</>))
import Text.Read (readMaybe)

import qualified Crypto.Hash                as Hash
import qualified DBT.Postgresql             as Postgresql
import qualified DBT.Session                as DBT
import qualified Data.ByteArray             as BA
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.Foldable              as Foldable
import qualified Data.List                  as List
import qualified Data.Text.IO               as Text
import qualified Hasql.Session              as Hasql
import qualified Hasql.Transaction          as Transaction
import qualified Hasql.Transaction.Sessions as Transaction
import qualified System.Environment         as Environment
import qualified System.Path                as Path
import qualified System.Path.Directory      as Path
import qualified System.Process.Typed       as Process

type Digest = Hash.Digest Hash.SHA3_256

data AppliedMigration = AppliedMigration
  { digest :: Digest
  , index  :: Natural
  }
  deriving stock Show

data MigrationFile = MigrationFile
  { digest :: Digest
  , index  :: Natural
  , path   :: Path.RelFile
  , sql    :: ByteString
  }

dryApply :: DBT.Session env => RIO env ()
dryApply = do
  files <- getNewMigrations

  if Foldable.null files
    then printStatus ("No new migrations to apply" :: Text)
    else Foldable.traverse_ printMigrationFile files

status :: DBT.Session env => RIO env ()
status = do
  applied <- readAppliedMigrations

  printStatus $ "Applied migrations: " <> show (Foldable.length applied)

  Foldable.traverse_ (printStatus . show) applied

  Foldable.traverse_ printMigrationFile =<<
    newMigrations applied <$> readMigrationFiles

dumpSchema :: Postgresql.ClientConfig -> RIO env ()
dumpSchema config = do
  printStatus $ "Writing schema to " <> schemaFileString
  env <- liftIO Environment.getEnvironment
  schema <- Process.readProcessStdout_
    . Process.setEnv (env <> Postgresql.toEnv config)
    $ Process.proc "pg_dump" ["--no-comments", "--schema-only"]
  migrations <- Process.readProcessStdout_
    . Process.setEnv (env <> Postgresql.toEnv config)
    $ Process.proc "pg_dump" ["--data-only", "--inserts", "--table=schema_migrations"]
  liftIO $ LBS.writeFile schemaFileString $ schema <> migrations

loadSchema :: DBT.Session env => RIO env ()
loadSchema = do
  printStatus $ "Loading schema from " <> schemaFileString
  runSession $ Transaction.transaction
    Transaction.Serializable
    Transaction.Write =<<
      Transaction.sql <$> liftIO (BS.readFile schemaFileString)

apply :: DBT.Session env => RIO env ()
apply = do
  migrations <- getNewMigrations
  printStatus $ "Applying " <> show (Foldable.length migrations) <> " pending migrations"
  Foldable.traverse_ applyMigration migrations

new :: DBT.Session env => RIO env ()
new = do
  applied <- max . fmap appliedIndex <$> readAppliedMigrations
  files   <- max . fmap fileIndex    <$> readMigrationFiles

  let index = succ $ max [applied, files]
      file  = Path.toString $ migrationDir </> Path.relFile (show index) <.> ".sql"

  printStatus $ "Creating new migration file: " <> file

  liftIO $ do
    Path.createDirectoryIfMissing True migrationDir
    BS.writeFile file ""

  where
    max :: (Foldable t, Num a, Ord a) => t a -> a
    max xs = if Foldable.null xs then 0 else Foldable.maximum xs

setup :: DBT.Session env => RIO env ()
setup = runSession $ Hasql.sql
  [uncheckedSql|
    CREATE TABLE IF NOT EXISTS
      schema_migrations
      ( index           int8  NOT NULL CHECK (index > 0)
      , digest_sha3_256 bytea NOT NULL CHECK (octet_length(digest_sha3_256) = 32)
      , PRIMARY KEY (index)
      )
  |]

printMigrationFile :: MigrationFile -> RIO env ()
printMigrationFile MigrationFile{..} = printStatus $ Path.toString path

applyMigration :: DBT.Session env => MigrationFile -> RIO env ()
applyMigration MigrationFile{..} = do
  printStatus $ "Applying migration: " <> Path.toString path
  runSession . Transaction.transaction Transaction.Serializable Transaction.Write $ do
    Transaction.sql sql

    Transaction.statement (BS.pack $ BA.unpack digest, convertUnsafe index)
      [resultlessStatement|
        INSERT INTO
          schema_migrations
          (digest_sha3_256, index)
        VALUES
          ($1 :: bytea, $2 :: int8)
      |]

  printStatus ("Success" :: Text)

readAppliedMigrations :: DBT.Session env => RIO env [AppliedMigration]
readAppliedMigrations = runSession $ do
  rows <- Foldable.toList <$> Hasql.statement ()
    [vectorStatement|
      SELECT
        digest_sha3_256 :: bytea
      , index           :: int8
      FROM
        schema_migrations
      ORDER BY
        index
    |]
  pure $ uncurry AppliedMigration . bimap fromByteString convertUnsafe <$> rows
  where
    fromByteString :: ByteString -> Hash.Digest Hash.SHA3_256
    fromByteString
      = maybe (error "Failed to decode sha3_256 from DB") identity
      . Hash.digestFromByteString

readMigrationFiles :: RIO env [MigrationFile]
readMigrationFiles = liftIO $
  whenM (Path.doesDirectoryExist migrationDir)
    $ Foldable.foldMap load =<< Path.filesInDir migrationDir
  where
    load :: Path.RelFile -> IO [MigrationFile]
    load file =
      whenM (pure $ isMigrationFile file)
        fmap pure . readMigration file =<< parseIndex file

    isMigrationFile :: Path.RelFile -> Bool
    isMigrationFile = (== ".sql") . Path.takeExtension

    parseIndex :: Path.RelFile -> IO Natural
    parseIndex file =
      maybe
        (fail $ "Invalid migration file name: " <> Path.toString file)
        pure
        (readMaybe . Path.toString $ Path.dropExtension file)

    readMigration :: Path.RelFile -> Natural -> IO MigrationFile
    readMigration path index = do
      sql <- BS.readFile . Path.toString $ migrationDir </> path
      pure MigrationFile
        { digest = Hash.hash sql
        , ..
        }

dbDir :: Path.RelDir
dbDir = Path.relDir "db"

migrationDir :: Path.RelDir
migrationDir = dbDir </> Path.relDir "migrations"

schemaFile :: Path.RelFile
schemaFile = dbDir </> Path.relFile "schema.sql"

getNewMigrations :: DBT.Session env => RIO env [MigrationFile]
getNewMigrations = newMigrations <$> readAppliedMigrations <*> readMigrationFiles

appliedIndex :: AppliedMigration -> Natural
appliedIndex = index

fileIndex :: MigrationFile -> Natural
fileIndex = index

schemaFileString :: String
schemaFileString = Path.toString schemaFile

newMigrations :: [AppliedMigration] -> [MigrationFile] -> [MigrationFile]
newMigrations applied = List.sortOn fileIndex . Foldable.foldMap test
  where
    test :: MigrationFile -> [MigrationFile]
    test file = guard' (not . isApplied $ fileIndex file) file

    isApplied :: Natural -> Bool
    isApplied fileIndex' = Foldable.any ((==) fileIndex' . appliedIndex) applied

printStatus :: ToText a => a -> RIO env ()
printStatus = liftIO . Text.putStrLn . convert
