module Database.Migration.CLI (WithClientConfig, parserInfo) where

import Database.Migration.Prelude
import Options.Applicative

import qualified DBT.Postgresql                as Postgresql
import qualified Database.Migration            as Migration
import qualified Database.Migration.Connection as Database
import qualified Hasql.Session                 as Hasql

type WithClientConfig = ((Postgresql.ClientConfig -> IO ()) -> IO ())

parserInfo :: ParserInfo (WithClientConfig -> IO ())
parserInfo = wrapHelper commands "migration commands"
  where
    commands :: Parser (WithClientConfig -> IO ())
    commands = hsubparser
      $  mkCommand
         "apply"
         applyMigration
         "Apply pending DB migrations"
      <> mkCommand
         "apply-dry"
         (withConnection Migration.dryApply)
         "Show pending migrations that would be applied"
      <> mkCommand
         "new"
         (withConnection Migration.new)
         "Create new migration file"
      <> mkCommand
         "status"
         (withConnection Migration.status)
         "Show DB migration status"
      <> mkCommand
         "setup"
         (withConnection Migration.setup)
         "Setup DB migrations, create schema_migrations table"
      <> mkCommand
         "dump-schema"
         (\withConfig -> withConfig Migration.dumpSchema)
         ("Dump database schema to " <> Migration.schemaFileString)
      <> mkCommand
         "load-schema"
         (withConnection Migration.loadSchema)
         ("Load database schema from " <> Migration.schemaFileString)

applyMigration
  :: WithClientConfig
  -> IO ()
applyMigration withConfig = do
  withConfig $ \config -> do
    Database.withConnection config $ eitherFail <=< Hasql.run Migration.apply
    Migration.dumpSchema config

withConnection
  :: Hasql.Session ()
  -> WithClientConfig
  -> IO ()
withConnection session withConfig =
  withConfig $ \config ->
    Database.withConnection config $ eitherFail <=< Hasql.run session

wrapHelper :: Parser b -> String -> ParserInfo b
wrapHelper parser desc = info parser $ progDesc desc

mkCommand
  :: String
  -> b
  -> String
  -> Mod CommandFields b
mkCommand name action' desc = command name $ wrapHelper (pure action') desc
