module Database.Migration.CLI (WithClientConfig, parserInfo) where

import Database.Migration.Prelude
import Options.Applicative

import qualified DBT.Connection     as DBT
import qualified DBT.Postgresql     as Postgresql
import qualified Database.Migration as Migration
import qualified Hasql.Session      as Hasql

type WithClientConfig = ((Postgresql.ClientConfig -> IO ()) -> IO ())

parserInfo :: Migration.LogInfo -> ParserInfo (WithClientConfig -> IO ())
parserInfo logInfo = wrapHelper commands "migration commands"
  where
    commands :: Parser (WithClientConfig -> IO ())
    commands = hsubparser
      $  mkCommand
         "apply"
         (applyMigration logInfo)
         "Apply pending DB migrations"
      <> mkCommand
         "apply-dry"
         (withConnection $ Migration.dryApply logInfo)
         "Show pending migrations that would be applied"
      <> mkCommand
         "new"
         (withConnection $ Migration.new logInfo)
         "Create new migration file"
      <> mkCommand
         "status"
         (withConnection $ Migration.status logInfo)
         "Show DB migration status"
      <> mkCommand
         "setup"
         (withConnection Migration.setup)
         "Setup DB migrations, create schema_migrations table"
      <> mkCommand
         "dump-schema"
         (\withConfig -> withConfig $ Migration.dumpSchema logInfo)
         ("Dump database schema to " <> Migration.schemaFileString)
      <> mkCommand
         "load-schema"
         (withConnection $ Migration.loadSchema logInfo)
         ("Load database schema from " <> Migration.schemaFileString)

applyMigration
  :: Migration.LogInfo
  -> WithClientConfig
  -> IO ()
applyMigration logInfo withConfig = do
  withConfig $ \config -> do
    DBT.withConnection config $ eitherFail <=< Hasql.run (Migration.apply logInfo)
    Migration.dumpSchema logInfo config

withConnection
  :: Hasql.Session ()
  -> WithClientConfig
  -> IO ()
withConnection session withConfig =
  withConfig $ \config ->
    DBT.withConnection config $ eitherFail <=< Hasql.run session

wrapHelper :: Parser b -> String -> ParserInfo b
wrapHelper parser desc = info parser $ progDesc desc

mkCommand
  :: String
  -> b
  -> String
  -> Mod CommandFields b
mkCommand name action' desc = command name $ wrapHelper (pure action') desc
