module DBT.CLI (main) where

import Control.Applicative (many)
import Control.Monad (join)
import DBT.Prelude
import System.IO (IO)

import qualified DBT.Backend           as Backend
import qualified Data.Text.IO         as Text
import qualified Options.Applicative  as CLI
import qualified System.Environment   as Environment
import qualified System.Process.Typed as Process

main :: IO ()
main = join $ parseCLI =<< Environment.getArgs

parseCLI :: forall m . MonadIO m => [String] -> m (m ())
parseCLI
  = liftIO
  . CLI.handleParseResult
  . CLI.execParserPure CLI.defaultPrefs root
  where
    root :: CLI.ParserInfo (m ())
    root = wrapHelper (CLI.helper <*> commands) "DBT commands"

    commands :: CLI.Parser (m ())
    commands =
      CLI.subparser
        $  envCommand
        <> localConfigCommand
        <> printImageCommand
        <> printMasterPasswordCommand
        <> printPortCommand
        <> runCommand
        <> startCommand
        <> statusCommand
        <> stopCommand

    envCommand :: CLI.Mod CLI.CommandFields (m ())
    envCommand =
      mkCommand
        "env"
        (runEnv <$> argument <*> arguments)
        "Run progam within env"

    localConfigCommand :: CLI.Mod CLI.CommandFields (m ())
    localConfigCommand =
      mkCommand
        "local-config"
        (pure (Backend.localConfig @'Backend.Podman))
        "Perform local database setup"

    printImageCommand :: CLI.Mod CLI.CommandFields (m ())
    printImageCommand =
      mkCommand
        "image"
        (pure $ print (Backend.getImage @'Backend.Podman))
        "Print image"

    printMasterPasswordCommand :: CLI.Mod CLI.CommandFields (m ())
    printMasterPasswordCommand =
      mkCommand
        "master-password"
        (pure $ print (Backend.getMasterPassword @'Backend.Podman))
        "Print master password"

    printPortCommand :: CLI.Mod CLI.CommandFields (m ())
    printPortCommand =
      mkCommand
        "port"
        (pure $ print (Backend.getHostPort @'Backend.Podman))
        "Print host port"

    runCommand :: CLI.Mod CLI.CommandFields (m ())
    runCommand =
      mkCommand
        "run"
        (Backend.run @'Backend.Podman <$> arguments)
        "Run command in new container"

    startCommand :: CLI.Mod CLI.CommandFields (m ())
    startCommand =
      mkCommand
        "start"
        (Backend.start @'Backend.Podman <$> detachOption <*> arguments)
        "Start database container"

    stopCommand :: CLI.Mod CLI.CommandFields (m ())
    stopCommand =
      mkCommand
        "stop"
        (pure (Backend.stop @'Backend.Podman))
        "Stop database container"

    statusCommand :: CLI.Mod CLI.CommandFields (m ())
    statusCommand =
      mkCommand
        "status"
        (pure $ print (Backend.status @'Backend.Podman))
        "Print status"

    argument :: CLI.Parser String
    argument = CLI.strArgument mempty

    wrapHelper :: CLI.Parser b -> String -> CLI.ParserInfo b
    wrapHelper parser desc = CLI.info parser $ CLI.progDesc desc

    mkCommand
      :: String
      -> CLI.Parser b
      -> String
      -> CLI.Mod CLI.CommandFields b
    mkCommand name parser desc = CLI.command name (wrapHelper parser desc)

    detachOption
      = CLI.flag Backend.Detach Backend.Foreground
      $ CLI.long "foreground" <> CLI.help "Start in foreground"

    arguments = many argument

print :: (MonadIO m, ToText a) => m a -> m ()
print action = liftIO . Text.putStrLn =<< convertText <$> action

runEnv :: MonadIO m => String -> [String] -> m ()
runEnv app arguments = Backend.withPostgresqlEnv @'Backend.Podman $ Process.proc app arguments
