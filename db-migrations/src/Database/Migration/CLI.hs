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

import qualified DBT.Postgresql            as Postgresql
import qualified DBT.Postgresql.Connection as DBT

type WithClientConfig env = forall a . (Postgresql.ClientConfig -> RIO env a) -> RIO env a

data Config env = Config
  { runPGDump  :: RunPGDump env
  , withConfig :: WithClientConfig env
  }

defaultConfig :: WithClientConfig env -> Config env
defaultConfig withConfig
  = Config
  { runPGDump = withConfig . localPGDump
  , ..
  }

parserInfo
  :: forall env . (HasMigrationEnv env, HasMigrationEnv (DBT.ConnectionEnv env))
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

    withConnection'
      :: RIO (DBT.ConnectionEnv env) ()
      -> Config env
      -> RIO env ()
    withConnection' = flip withConnection

applyMigration
  :: (HasMigrationEnv env, HasMigrationEnv (DBT.ConnectionEnv env))
  => Config env
  -> RIO env ()
applyMigration Config{..} = do
  withConfig $ \config ->
    DBT.withConnection config $ DBT.runConnectionEnv apply
  dumpSchema runPGDump

withConnection
  :: Config env
  -> RIO (DBT.ConnectionEnv env) ()
  -> RIO env ()
withConnection Config{..} action' =
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
