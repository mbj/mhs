module Database.Migration.CLI (parserInfo) where

import Database.Migration.Prelude
import Options.Applicative

import qualified DBT.Postgresql                as Postgresql
import qualified Database.Migration            as Migration
import qualified Database.Migration.Connection as Database
import qualified Hasql.Session                 as Hasql

parserInfo
  :: ((Postgresql.ClientConfig -> IO ()) -> IO ())
  -> ParserInfo (IO ())
parserInfo withConfig = wrapHelper commands "migration commands"
  where
    commands :: Parser (IO ())
    commands = hsubparser
      $  mkCommand
         "apply"
         (applyMigration withConfig)
         "Apply pending DB migrations"
      <> mkCommand
         "apply-dry"
         (withConnection withConfig Migration.dryApply)
         "Show pending migrations that would be applied"
      <> mkCommand
         "new"
         (withConnection withConfig Migration.new)
         "Create new migration file"
      <> mkCommand
         "status"
         (withConnection withConfig Migration.status)
         "Show DB migration status"
      <> mkCommand
         "setup"
         (withConnection withConfig Migration.setup)
         "Setup DB migrations, create schema_migrations table"
      <> mkCommand
         "dump-schema"
         (withConfig $ liftIO . Migration.dumpSchema)
         ("Dump database schema to " <> Migration.schemaFileString)
      <> mkCommand
         "load-schema"
         (withConnection withConfig Migration.loadSchema)
         ("Load database schema from " <> Migration.schemaFileString)

applyMigration
  :: ((Postgresql.ClientConfig -> IO ()) -> IO ())
  -> IO ()
applyMigration withConfig = do
  withConfig $ \config -> liftIO $ do
    Database.withConnection config $ eitherFail <=< Hasql.run Migration.apply
    Migration.dumpSchema config

withConnection
  :: ((Postgresql.ClientConfig -> IO a) -> IO a)
  -> Hasql.Session a
  -> IO a
withConnection withConfig session =
  withConfig $ \config ->
    liftIO $ Database.withConnection config $ eitherFail <=< Hasql.run session

wrapHelper :: Parser b -> String -> ParserInfo b
wrapHelper parser desc = info parser $ progDesc desc

mkCommand
  :: String
  -> b
  -> String
  -> Mod CommandFields b
mkCommand name action' desc = command name $ wrapHelper (pure action') desc
