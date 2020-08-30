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
import qualified Data.ByteArray             as BA
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.Foldable              as Foldable
import qualified Data.List                  as List
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

dryApply :: Hasql.Session ()
dryApply = do
  files <- getNewMigrations

  if Foldable.null files
    then log @Text "No new migrations to apply"
    else Foldable.traverse_ (liftIO . printMigrationFile) files

status :: Hasql.Session ()
status = do
  applied <- readAppliedMigrations

  log $ "Applied migrations: " <> show (Foldable.length applied)

  liftIO $ do
    Foldable.traverse_ (log . show) applied

    Foldable.traverse_ printMigrationFile =<<
      newMigrations applied <$> readMigrationFiles

dumpSchema :: Postgresql.ClientConfig -> IO ()
dumpSchema config = do
  log $ "Writing schema to " <> schemaFileString
  env <- Environment.getEnvironment
  schema <- Process.readProcessStdout_
    . Process.setEnv (env <> Postgresql.toEnv config)
    $ Process.proc "pg_dump" ["--no-comments", "--schema-only"]
  migrations <- Process.readProcessStdout_
    . Process.setEnv (env <> Postgresql.toEnv config)
    $ Process.proc "pg_dump" ["--data-only", "--inserts", "--table=schema_migrations"]
  LBS.writeFile schemaFileString $ schema <> migrations

loadSchema :: Hasql.Session ()
loadSchema = do
  log $ "Loading schema from " <> schemaFileString
  Transaction.transaction
    Transaction.Serializable
    Transaction.Write =<<
      Transaction.sql <$> liftIO (BS.readFile schemaFileString)

apply :: Hasql.Session ()
apply = do
  migrations <- getNewMigrations
  log $ "Applying " <> show (Foldable.length migrations) <> " pending migrations"
  Foldable.traverse_ applyMigration migrations

new :: Hasql.Session ()
new = do
  applied <- max . fmap appliedIndex <$> readAppliedMigrations
  files   <- max . fmap fileIndex    <$> liftIO readMigrationFiles

  let index = succ $ max [applied, files]
      file  = Path.toString $ migrationDir </> Path.relFile (show index) <.> ".sql"

  log $ "Creating new migration file: " <> file

  liftIO $ do
    Path.createDirectoryIfMissing True migrationDir
    BS.writeFile file ""

  where
    max :: (Foldable t, Num a, Ord a) => t a -> a
    max xs = if Foldable.null xs then 0 else Foldable.maximum xs

setup :: Hasql.Session ()
setup = Hasql.sql
  [uncheckedSql|
    CREATE TABLE IF NOT EXISTS
      schema_migrations
      ( index           int8  NOT NULL CHECK (index > 0)
      , digest_sha3_256 bytea NOT NULL CHECK (octet_length(digest_sha3_256) = 32)
      , PRIMARY KEY (index)
      )
  |]

printMigrationFile :: MigrationFile -> IO ()
printMigrationFile MigrationFile{..} = log $ Path.toString path

applyMigration :: MigrationFile -> Hasql.Session ()
applyMigration MigrationFile{..} = do
  log $ "Applying migration: " <> Path.toString path
  Transaction.transaction Transaction.Serializable Transaction.Write $ do
    Transaction.sql sql

    Transaction.statement (BS.pack $ BA.unpack digest, convertUnsafe index)
      [resultlessStatement|
        INSERT INTO
          schema_migrations
          (digest_sha3_256, index)
        VALUES
          ($1 :: bytea, $2 :: int8)
      |]
  log @Text "Success"

readAppliedMigrations :: Hasql.Session [AppliedMigration]
readAppliedMigrations = do
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

readMigrationFiles :: IO [MigrationFile]
readMigrationFiles =
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

newMigrations :: [AppliedMigration] -> [MigrationFile] -> [MigrationFile]
newMigrations applied = List.sortOn fileIndex . Foldable.foldMap test
  where
    test :: MigrationFile -> [MigrationFile]
    test file = guard' (not . isApplied $ fileIndex file) file

    isApplied :: Natural -> Bool
    isApplied fileIndex' = Foldable.any ((==) fileIndex' . appliedIndex) applied

dbDir :: Path.RelDir
dbDir = Path.relDir "db"

migrationDir :: Path.RelDir
migrationDir = dbDir </> Path.relDir "migrations"

schemaFile :: Path.RelFile
schemaFile = dbDir </> Path.relFile "schema.sql"

getNewMigrations :: Hasql.Session [MigrationFile]
getNewMigrations = newMigrations <$> readAppliedMigrations <*> liftIO readMigrationFiles

appliedIndex :: AppliedMigration -> Natural
appliedIndex = index

fileIndex :: MigrationFile -> Natural
fileIndex = index

schemaFileString :: String
schemaFileString = Path.toString schemaFile
