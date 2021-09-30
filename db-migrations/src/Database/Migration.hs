{-# LANGUAGE QuasiQuotes #-}

module Database.Migration
  ( HasMigrationEnv
  , RunPGDump
  , apply
  , dryApply
  , dumpSchema
  , getSchemaFileString
  , loadSchema
  , localPGDump
  , new
  , setup
  , status
  )
where

import Control.Bool (whenM)
import Control.Monad.Reader (asks)
import Data.Bifunctor (bimap)
import Database.Migration.Prelude
import GHC.Enum (succ)
import GHC.Records (HasField(..))
import Hasql.TH (resultlessStatement, uncheckedSql, vectorStatement)
import Prelude (error)
import System.Path ((<.>), (</>))
import Text.Read (readMaybe)
import UnliftIO.Exception (throwString)

import qualified Crypto.Hash                as Hash
import qualified DBT.Postgresql             as Postgresql
import qualified DBT.Postgresql.Session     as DBT
import qualified Data.ByteArray             as BA
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.Foldable              as Foldable
import qualified Data.List                  as List
import qualified Data.Text.IO               as Text
import qualified Hasql.Session              as Hasql
import qualified Hasql.Transaction          as Transaction
import qualified Hasql.Transaction.Sessions as Transaction
import qualified System.Path                as Path
import qualified System.Path.Directory      as Path
import qualified System.Process.Typed       as Process

type Digest = Hash.Digest Hash.SHA3_256

type RunPGDump env = [Text] -> RIO env LBS.ByteString

type HasMigrationEnv env
  = ( HasField "migrationDir" env Path.RelDir
    , HasField "schemaFile"   env Path.RelFile
    )

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

dryApply :: (HasMigrationEnv env, DBT.Session env) => RIO env ()
dryApply = do
  files <- getNewMigrations

  if Foldable.null files
    then printStatus ("No new migrations to apply" :: Text)
    else Foldable.traverse_ printMigrationFile files

status :: (HasMigrationEnv env, DBT.Session env) => RIO env ()
status = do
  applied <- readAppliedMigrations

  printStatus $ "Applied migrations: " <> show (Foldable.length applied)

  Foldable.traverse_ (printStatus . show) applied

  Foldable.traverse_ printMigrationFile =<<
    newMigrations applied <$> readMigrationFiles


localPGDump :: [Text] -> Postgresql.ClientConfig -> RIO env LBS.ByteString
localPGDump arguments clientConfig =  do
  env <- Postgresql.getEnv clientConfig
  Process.readProcessStdout_
    . Process.setEnv env
    $ Process.proc "pg_dump" (convert <$> arguments)

dumpSchema :: HasMigrationEnv env => RunPGDump env -> RIO env ()
dumpSchema pgDump = do
  schemaFileString <- getSchemaFileString
  printStatus $ "Writing schema to " <> schemaFileString
  schema     <- pgDump ["--no-comments", "--schema-only"]
  migrations <- pgDump ["--data-only", "--inserts", "--table=schema_migrations"]
  liftIO $ LBS.writeFile schemaFileString $ schema <> migrations

loadSchema :: (HasMigrationEnv env, DBT.Session env) => RIO env ()
loadSchema = do
  schemaFileString <- getSchemaFileString
  printStatus $ "Loading schema from " <> schemaFileString
  DBT.runSession $ Transaction.transaction
    Transaction.Serializable
    Transaction.Write =<<
      Transaction.sql <$> liftIO (BS.readFile schemaFileString)

apply :: (HasMigrationEnv env, DBT.Session env) => RIO env ()
apply = do
  migrations <- getNewMigrations
  printStatus $ "Applying " <> show (Foldable.length migrations) <> " pending migrations"
  Foldable.traverse_ applyMigration migrations

new :: (HasMigrationEnv env, DBT.Session env) => RIO env ()
new = do
  applied       <- max . fmap appliedIndex <$> readAppliedMigrations
  files         <- max . fmap fileIndex    <$> readMigrationFiles
  migrationDir  <- getMigrationDir

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
setup = DBT.runSession $ Hasql.sql
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
  DBT.runSession . Transaction.transaction Transaction.Serializable Transaction.Write $ do
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
readAppliedMigrations = DBT.runSession $ do
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
    fromByteString :: BS.ByteString -> Hash.Digest Hash.SHA3_256
    fromByteString
      = maybe (error "Failed to decode sha3_256 from DB") identity
      . Hash.digestFromByteString

readMigrationFiles
  :: forall env . HasMigrationEnv env
  => RIO env [MigrationFile]
readMigrationFiles = do
  migrationDir <- getMigrationDir
  whenM (liftIO (Path.doesDirectoryExist migrationDir))
    $ Foldable.foldMap load =<< liftIO (Path.filesInDir migrationDir)
  where
    load :: Path.RelFile ->  RIO env [MigrationFile]
    load file =
      whenM (pure $ isMigrationFile file)
        fmap pure . readMigration file =<< parseIndex file

    isMigrationFile :: Path.RelFile -> Bool
    isMigrationFile = (== ".sql") . Path.takeExtension

    parseIndex :: Path.RelFile -> RIO env Natural
    parseIndex file =
      maybe
        (throwString $ "Invalid migration file name: " <> Path.toString file)
        pure
        (readMaybe . Path.toString $ Path.dropExtension file)

    readMigration :: Path.RelFile -> Natural -> RIO env MigrationFile
    readMigration path index = do
      migrationDir <- getMigrationDir
      sql <- liftIO $ BS.readFile . Path.toString $ migrationDir </> path
      pure MigrationFile
        { digest = Hash.hash sql
        , ..
        }


getMigrationDir :: HasMigrationEnv env => RIO env Path.RelDir
getMigrationDir = asks (getField @"migrationDir")

getNewMigrations :: (HasMigrationEnv env, DBT.Session env) => RIO env [MigrationFile]
getNewMigrations = newMigrations <$> readAppliedMigrations <*> readMigrationFiles

appliedIndex :: AppliedMigration -> Natural
appliedIndex = index

fileIndex :: MigrationFile -> Natural
fileIndex = index

getSchemaFileString :: HasMigrationEnv env => RIO env String
getSchemaFileString = Path.toString <$> asks (getField @"schemaFile")

newMigrations :: [AppliedMigration] -> [MigrationFile] -> [MigrationFile]
newMigrations applied = List.sortOn fileIndex . Foldable.foldMap test
  where
    test :: MigrationFile -> [MigrationFile]
    test file = guard' (not . isApplied $ fileIndex file) file

    isApplied :: Natural -> Bool
    isApplied fileIndex' = Foldable.any ((==) fileIndex' . appliedIndex) applied

printStatus :: ToText a => a -> RIO env ()
printStatus = liftIO . Text.putStrLn . convert
