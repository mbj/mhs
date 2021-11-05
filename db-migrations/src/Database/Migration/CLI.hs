module Database.Migration.CLI (parserInfo) where

import Database.Migration
import Database.Migration.Prelude
import Options.Applicative

parserInfo
  :: forall env . Env env
  => ParserInfo (DynamicConfig env -> RIO env ())
parserInfo = wrapHelper commands "migration commands"
  where
    commands :: Parser (DynamicConfig env -> RIO env ())
    commands = hsubparser
      $  mkCommand
         "apply"
         applyDump
         "Apply pending DB migrations with dumping new schema"
      <>  mkCommand
         "apply-no-schema"
         (withConnection' apply)
         "Apply pending DB migrations without dumping new schema"
      <> mkCommand
         "apply-dry"
         (withConnection' dryApply)
         "Show pending migrations that would be applied"
      <> mkCommand
         "apply-dry"
         (withConnection' dryApply)
         "Show pending migrations that would be applied"
      <> mkCommand
         "new"
         (withConnection' new)
         "Create new migration file"
      <> mkCommand
         "status"
         (withConnection' status)
         "Show DB migration status"
      <> mkCommand
         "setup"
         (withConnection' setup)
         "Setup DB migrations, create schema_migrations table"
      <> mkCommand
         "dump-schema"
         dumpSchema
         "Dump database schema"
      <> mkCommand
         "load-schema"
         (withConnection' loadSchema)
         "Load database schema"

    withConnection' :: RIO ConnectionEnv a -> DynamicConfig env -> RIO env a
    withConnection' = flip withDynamicConnectionEnv

wrapHelper :: Parser b -> String -> ParserInfo b
wrapHelper parser desc = info parser $ progDesc desc

mkCommand
  :: String
  -> b
  -> String
  -> Mod CommandFields b
mkCommand name action' desc = command name $ wrapHelper (pure action') desc
