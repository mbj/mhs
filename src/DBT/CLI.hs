module DBT.CLI (main) where

import Control.Applicative (many)
import Control.Monad (join)
import DBT.Prelude
import System.IO (IO)

import qualified DBT.Podman           as Podman
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
    root = wrapHelper commands "DBT commands"

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
        (pure Podman.localConfig)
        "Perform local database setup"

    printImageCommand :: CLI.Mod CLI.CommandFields (m ())
    printImageCommand =
      mkCommand
        "image"
        (pure $ print Podman.getImage)
        "Print image"

    printMasterPasswordCommand :: CLI.Mod CLI.CommandFields (m ())
    printMasterPasswordCommand =
      mkCommand
        "master-password"
        (pure $ print Podman.getMasterPassword)
        "Print master password"

    printPortCommand :: CLI.Mod CLI.CommandFields (m ())
    printPortCommand =
      mkCommand
        "port"
        (pure $ print Podman.getHostPort)
        "Print host port"

    runCommand :: CLI.Mod CLI.CommandFields (m ())
    runCommand =
      mkCommand
        "run"
        (Podman.run <$> arguments)
        "Run command in new container"

    startCommand :: CLI.Mod CLI.CommandFields (m ())
    startCommand =
      mkCommand
        "start"
        (Podman.start <$> detachOption <*> arguments)
        "Start database container"

    stopCommand :: CLI.Mod CLI.CommandFields (m ())
    stopCommand =
      mkCommand
        "stop"
        (pure Podman.stop)
        "Stop database container"

    statusCommand :: CLI.Mod CLI.CommandFields (m ())
    statusCommand =
      mkCommand
        "status"
        (pure $ print Podman.status)
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
      = CLI.flag Podman.Detach Podman.Foreground
      $ CLI.long "foreground" <> CLI.help "Start in foreground"

    arguments = many argument

print :: (MonadIO m, ToText a) => m a -> m ()
print action = liftIO . Text.putStrLn =<< convertText <$> action

runEnv :: MonadIO m => String -> [String] -> m ()
runEnv app arguments = Podman.withPostgresqlEnv $ Process.proc app arguments
