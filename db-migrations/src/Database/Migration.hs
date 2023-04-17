{-# LANGUAGE QuasiQuotes #-}

module Database.Migration
  ( ConnectionEnv(..)
  , DynamicConfig(..)
  , Env
  , RunPGDump
  , WithClientConfig
  , apply
  , applyDump
  , defaultDynamicConfig
  , dryApply
  , dumpSchema
  , getSchemaFileString
  , loadSchema
  , localPGDump
  , new
  , setup
  , status
  , withConnectionEnv
  , withDynamicConnectionEnv
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
import qualified DBT.Postgresql.Connection  as Connection
import qualified Data.ByteArray             as BA
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.Foldable              as Foldable
import qualified Data.List                  as List
import qualified Data.Text.IO               as Text
import qualified Hasql.Connection           as Hasql
import qualified Hasql.Session              as Hasql
import qualified Hasql.Transaction          as Transaction
import qualified Hasql.Transaction.Sessions as Transaction
import qualified System.Path                as Path
import qualified System.Path.Directory      as Path
import qualified System.Process.Typed       as Process

type Digest = Hash.Digest Hash.SHA3_256

type WithClientConfig env = forall a . (Postgresql.ClientConfig -> MIO env a) -> MIO env a

type RunPGDump env = [Text] -> MIO env LBS.ByteString

type Env env
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


data DynamicConfig env = DynamicConfig
  { runPGDump  :: RunPGDump env
  , withClientConfig :: WithClientConfig env
  }

data ConnectionEnv = ConnectionEnv
  { hasqlConnection :: Hasql.Connection
  , migrationDir    :: Path.RelDir
  , schemaFile      :: Path.RelFile
  }

dryApply :: (Env env, Connection.Env env) => MIO env ()
dryApply = do
  files <- getNewMigrations

  if Foldable.null files
    then printStatus ("No new migrations to apply" :: Text)
    else Foldable.traverse_ printMigrationFile files

status :: (Env env, Connection.Env env) => MIO env ()
status = do
  applied <- readAppliedMigrations

  printStatus $ "Applied migrations: " <> show (Foldable.length applied)

  Foldable.traverse_ (printStatus . show) applied

  Foldable.traverse_ printMigrationFile . newMigrations applied =<< readMigrationFiles


localPGDump :: [Text] -> Postgresql.ClientConfig -> MIO env LBS.ByteString
localPGDump arguments clientConfig =  do
  env <- Postgresql.getEnv clientConfig
  Process.readProcessStdout_
    . Process.setEnv env
    $ Process.proc "pg_dump" (convert <$> arguments)

dumpSchema :: Env env => DynamicConfig env -> MIO env ()
dumpSchema DynamicConfig{..} = do
  schemaFileString <- getSchemaFileString
  printStatus $ "Writing schema to " <> schemaFileString
  schema     <- runPGDump ["--no-comments", "--schema-only"]
  migrations <- runPGDump ["--data-only", "--inserts", "--table=schema_migrations"]
  liftIO $ LBS.writeFile schemaFileString $ schema <> migrations

loadSchema :: (Env env, Connection.Env env) => MIO env ()
loadSchema = do
  schemaFileString <- getSchemaFileString
  printStatus $ "Loading schema from " <> schemaFileString
  Connection.runSession $ Transaction.transaction
    Transaction.Serializable
    Transaction.Write . Transaction.sql =<< liftIO (BS.readFile schemaFileString)

apply :: (Env env, Connection.Env env) => MIO env ()
apply = do
  migrations <- getNewMigrations
  printStatus $ "Applying " <> show (Foldable.length migrations) <> " pending migrations"
  Foldable.traverse_ applyMigration migrations

new :: (Env env, Connection.Env env) => MIO env ()
new = do
  applied       <- max . fmap appliedIndex <$> readAppliedMigrations
  files         <- max . fmap fileIndex    <$> readMigrationFiles
  migrationDir  <- getMigrationDir

  let index = succ $ max ([applied, files] :: [Natural])
      file  = Path.toString $ migrationDir </> Path.relFile (show index) <.> ".sql"

  printStatus $ "Creating new migration file: " <> file

  liftIO $ do
    Path.createDirectoryIfMissing True migrationDir
    BS.writeFile file ""

  where
    max :: (Foldable t, Num a, Ord a) => t a -> a
    max xs = if Foldable.null xs then 0 else Foldable.maximum xs

setup :: Connection.Env env => MIO env ()
setup = Connection.runSession $ Hasql.sql
  [uncheckedSql|
    CREATE TABLE IF NOT EXISTS
      schema_migrations
      ( index           int8  NOT NULL CHECK (index > 0)
      , digest_sha3_256 bytea NOT NULL CHECK (octet_length(digest_sha3_256) = 32)
      , PRIMARY KEY (index)
      )
  |]

printMigrationFile :: MigrationFile -> MIO env ()
printMigrationFile MigrationFile{..} = printStatus $ Path.toString path

applyMigration :: Connection.Env env => MigrationFile -> MIO env ()
applyMigration MigrationFile{..} = do
  printStatus $ "Applying migration: " <> Path.toString path
  Connection.runSession . Transaction.transaction Transaction.Serializable Transaction.Write $ do
    Transaction.sql sql

    Transaction.statement (BS.pack $ BA.unpack digest, convertImpure index)
      [resultlessStatement|
        INSERT INTO
          schema_migrations
          (digest_sha3_256, index)
        VALUES
          ($1 :: bytea, $2 :: int8)
      |]

  printStatus ("Success" :: Text)

readAppliedMigrations :: Connection.Env env => MIO env [AppliedMigration]
readAppliedMigrations = Connection.runSession $ do
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
  pure $ uncurry AppliedMigration . bimap fromByteString convertImpure <$> rows
  where
    fromByteString :: BS.ByteString -> Hash.Digest Hash.SHA3_256
    fromByteString
      = maybe (error "Failed to decode sha3_256 from DB") identity
      . Hash.digestFromByteString

readMigrationFiles
  :: forall env . Env env
  => MIO env [MigrationFile]
readMigrationFiles = do
  migrationDir <- getMigrationDir
  whenM (liftIO (Path.doesDirectoryExist migrationDir))
    $ Foldable.foldMap load =<< liftIO (Path.filesInDir migrationDir)
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
      migrationDir <- getMigrationDir
      sql <- liftIO $ BS.readFile . Path.toString $ migrationDir </> path
      pure MigrationFile
        { digest = Hash.hash sql
        , ..
        }


getMigrationDir :: Env env => MIO env Path.RelDir
getMigrationDir = asks (.migrationDir)

getNewMigrations :: (Env env, Connection.Env env) => MIO env [MigrationFile]
getNewMigrations = newMigrations <$> readAppliedMigrations <*> readMigrationFiles

appliedIndex :: AppliedMigration -> Natural
appliedIndex = (.index)

fileIndex :: MigrationFile -> Natural
fileIndex = (.index)

getSchemaFileString :: Env env => MIO env String
getSchemaFileString = asks (Path.toString . (.schemaFile))

newMigrations :: [AppliedMigration] -> [MigrationFile] -> [MigrationFile]
newMigrations applied = List.sortOn fileIndex . Foldable.foldMap test
  where
    test :: MigrationFile -> [MigrationFile]
    test file = guard' (not . isApplied $ fileIndex file) file

    isApplied :: Natural -> Bool
    isApplied fileIndex' = Foldable.any ((==) fileIndex' . appliedIndex) applied

printStatus :: ToText a => a -> MIO env ()
printStatus = liftIO . Text.putStrLn . convert

withConnectionEnv
  :: Env env
  => Postgresql.ClientConfig
  -> MIO ConnectionEnv a
  -> MIO env a
withConnectionEnv clientConfig action' = do
  migrationDir <- asks (.migrationDir)
  schemaFile   <- asks (.schemaFile)

  Connection.withConnection clientConfig $ \hasqlConnection ->
    runMIO ConnectionEnv{..} action'

withDynamicConnectionEnv
  :: Env env
  => DynamicConfig env
  -> MIO ConnectionEnv a
  -> MIO env a
withDynamicConnectionEnv DynamicConfig{..} action = do
  withClientConfig $ \clientConfig ->
    withConnectionEnv clientConfig action

defaultDynamicConfig :: WithClientConfig env -> DynamicConfig env
defaultDynamicConfig withClientConfig
  = DynamicConfig
  { runPGDump = withClientConfig . localPGDump
  , ..
  }

applyDump
  :: Env env
  => DynamicConfig env
  -> MIO env ()
applyDump dynamicConfig = do
  withDynamicConnectionEnv dynamicConfig apply
  dumpSchema dynamicConfig
