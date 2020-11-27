module DBT.Postgresql.Container.CLI (main) where

import Control.Monad (join)
import DBT.Postgresql.Container
import DBT.Postgresql.Prelude
import Options.Applicative

import qualified CBT
import qualified System.Environment   as Environment
import qualified System.IO            as IO
import qualified System.Process.Typed as Process

main :: IO ()
main = do
  arguments <- Environment.getArgs
  CBT.runDefaultEnvironment $ run arguments

run
  :: forall env . (CBT.WithEnv env)
  => [String]
  -> RIO env ()
run arguments = do
  liftIO $ IO.hSetBuffering IO.stdout IO.LineBuffering
  join
    . liftIO
    . handleParseResult
    $ execParserPure preferences parser arguments
  where
    preferences = prefs showHelpOnEmpty

    parser :: ParserInfo (RIO env ())
    parser = wrapHelper "dbt commands" commands

    commands :: Parser (RIO env ())
    commands
      = hsubparser
      $ mkCommand
        "run-ephemeral"
        "run program with ephemeral database"
        (withDatabaseContainerProcessRun_ prefix <$> processProc)

    prefix :: CBT.Prefix
    prefix = CBT.Prefix "dbt"

    processProc :: Parser (Process.ProcessConfig () () ())
    processProc = Process.proc <$> programName <*> programArguments

    programName :: Parser String
    programName
      = strArgument
      $ metavar "COMMAND"

    programArguments :: Parser [String]
    programArguments
      = many
      . strArgument
      $ metavar "ARGUMENT"

mkCommand :: String -> String -> Parser a -> Mod CommandFields a
mkCommand name description parser =
  command name (wrapHelper description parser)

wrapHelper :: String -> Parser b -> ParserInfo b
wrapHelper description parser = info parser (progDesc description)
