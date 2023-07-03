module DBT.Migration.CLI (parserInfo) where

import DBT.Migration
import DBT.Prelude

import qualified Options.Applicative as CLI
import qualified System.Exit         as System

parserInfo
  :: forall env databaseInstance . Env env databaseInstance
  => CLI.ParserInfo (MIO env System.ExitCode)
parserInfo = wrapHelper commands "migration commands"
  where
    commands :: CLI.Parser (MIO env System.ExitCode)
    commands = CLI.hsubparser
      $  mkCommand
           "apply"
           (run applyPendingDumpSchema)
           "Apply pending DB migrations with dumping new schema"
      <> mkCommand
           "apply-no-dump"
           (run applyPending)
           "Apply pending DB migrations without dumping new schema"
      <> mkCommand
           "apply-dry"
           (run dryApply)
           "Show pending migrations that would be applied"
      <> mkCommand
           "create"
           (run create)
           "Create new migration"
      <> mkCommand
           "dump-schema"
           (run dumpSchema)
           "Dump database schema"
      <> mkCommand
           "load-schema"
           (run loadSchema)
           "Load database schema"
      <> mkCommand
           "setup"
           (run setup)
           "Setup DB migrations, create schema_migrations table"
      <> mkCommand
           "status"
           (run printStatus)
           "Show DB migration status"

    run :: MIO env a -> MIO env System.ExitCode
    run action = action $> System.ExitSuccess

wrapHelper :: CLI.Parser a -> String -> CLI.ParserInfo a
wrapHelper parser desc = CLI.info parser $ CLI.progDesc desc

mkCommand
  :: String
  -> a
  -> String
  -> CLI.Mod CLI.CommandFields a
mkCommand name action = CLI.command name . wrapHelper (pure action)
