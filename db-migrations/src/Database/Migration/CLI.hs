module Database.Migration.CLI
  ( Config(..)
  , WithClientConfig
  , defaultConfig
  , parserInfo
  )
where

import Database.Migration
import Database.Migration.Prelude
import Options.Applicative

defaultConfig :: WithClientConfig env -> Config env
defaultConfig withConfig
  = Config
  { runPGDump = withConfig . localPGDump
  , ..
  }

parserInfo
  :: forall env . Env env
  => ParserInfo (Config env -> RIO env ())
parserInfo = wrapHelper commands "migration commands"
  where
    commands :: Parser (Config env -> RIO env ())
    commands = hsubparser
      $  mkCommand
         "apply"
         applyMigration
         "Apply pending DB migrations"
      <>  mkCommand
         "apply-no-schema"
         (withConnection' apply)
         "Apply pending DB migrations"
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
         (\Config{..} -> dumpSchema runPGDump)
         "Dump database schema"
      <> mkCommand
         "load-schema"
         (withConnection' loadSchema)
         "Load database schema"

    withConnection' :: RIO ConnectionEnv a -> Config env -> RIO env a
    withConnection' = flip withConnectionEnv

applyMigration
  :: Env env
  => Config env
  -> RIO env ()
applyMigration config@Config{..} = do
  withConnectionEnv config apply
  dumpSchema runPGDump

wrapHelper :: Parser b -> String -> ParserInfo b
wrapHelper parser desc = info parser $ progDesc desc

mkCommand
  :: String
  -> b
  -> String
  -> Mod CommandFields b
mkCommand name action' desc = command name $ wrapHelper (pure action') desc
