module Database.Migration.CLI (parserInfo) where

import Database.Migration.Prelude
import Options.Applicative

import qualified DBT.Postgresql                as Postgresql
import qualified Database.Migration            as Migration
import qualified Database.Migration.Connection as Database
import qualified Hasql.Session                 as Hasql

parserInfo
  :: forall m . MonadUnliftIO m
  => m Postgresql.ClientConfig
  -> ParserInfo (m ())
parserInfo getConfig = wrapHelper commands "migration commands"
  where
    commands :: Parser (m ())
    commands = hsubparser
      $  mkCommand
         "apply"
         (applyMigration getConfig)
         "Apply pending DB migrations"
      <>  mkCommand
         "apply-dry"
         (withConnection getConfig Migration.dryApply)
         "Show pending migrations that would be applied"
      <> mkCommand
         "new"
         (withConnection getConfig Migration.new)
         "Create new migration file"
      <> mkCommand
         "status"
         (withConnection getConfig Migration.status)
         "Show DB migration status"
      <> mkCommand
         "setup"
         (withConnection getConfig Migration.setup)
         "Setup DB migrations, create schema_migrations table"
      <> mkCommand
         "dump-schema"
         (withConfig getConfig Migration.dumpSchema)
         ("Dump database schema to " <> Migration.schemaFileString)
      <> mkCommand
         "load-schema"
         (withConnection getConfig Migration.loadSchema)
         ("Load database schema from " <> Migration.schemaFileString)

applyMigration
  :: MonadUnliftIO m
  => m Postgresql.ClientConfig
  -> m ()
applyMigration getConfig = do
  config <- getConfig
  liftIO $ do
    Database.withConnection config $ eitherFail <=< Hasql.run Migration.apply
    Migration.dumpSchema config

withConnection
  :: MonadUnliftIO m
  => m Postgresql.ClientConfig
  -> Hasql.Session a
  -> m a
withConnection getConfig session = do
  config <- getConfig
  liftIO $ Database.withConnection config $ eitherFail <=< Hasql.run session

withConfig
  :: MonadUnliftIO m
  => m Postgresql.ClientConfig
  -> (Postgresql.ClientConfig -> IO a)
  -> m a
withConfig getConfig action' = (liftIO . action') =<< getConfig

wrapHelper :: Parser b -> String -> ParserInfo b
wrapHelper parser desc = info parser $ progDesc desc

mkCommand
  :: String
  -> b
  -> String
  -> Mod CommandFields b
mkCommand name action' desc = command name $ wrapHelper (pure action') desc
