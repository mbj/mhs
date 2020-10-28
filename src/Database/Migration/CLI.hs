module Database.Migration.CLI
  ( WithClientConfig
  , parserInfo
  )
where

import Database.Migration
import Database.Migration.Prelude
import Options.Applicative

import qualified DBT.Connection as DBT
import qualified DBT.Postgresql as Postgresql

type WithClientConfig env = (Postgresql.ClientConfig -> RIO env ()) -> RIO env ()

parserInfo :: ParserInfo (WithClientConfig env -> RIO env ())
parserInfo = wrapHelper commands "migration commands"
  where
    commands :: Parser (WithClientConfig env -> RIO env ())
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
         (\withConfig -> withConfig dumpSchema)
         ("Dump database schema to " <> schemaFileString)
      <> mkCommand
         "load-schema"
         (withConnection' loadSchema)
         ("Load database schema from " <> schemaFileString)

    withConnection'
      :: RIO (DBT.ConnectionEnv env) ()
      -> WithClientConfig env
      -> RIO env ()
    withConnection' = flip withConnection

applyMigration
  :: WithClientConfig env
  -> RIO env ()
applyMigration withConfig =
  withConfig $ \config ->
    DBT.withConnection config $ DBT.runConnectionEnv (apply >> dumpSchema config)

withConnection
  :: WithClientConfig env
  -> RIO (DBT.ConnectionEnv env) ()
  -> RIO env ()
withConnection withConfig action' =
  withConfig $ \config ->
    DBT.withConnection config $ DBT.runConnectionEnv action'

wrapHelper :: Parser b -> String -> ParserInfo b
wrapHelper parser desc = info parser $ progDesc desc

mkCommand
  :: String
  -> b
  -> String
  -> Mod CommandFields b
mkCommand name action' desc = command name $ wrapHelper (pure action') desc
