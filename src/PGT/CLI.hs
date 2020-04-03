{-# LANGUAGE ExistentialQuantification #-}

module PGT.CLI (Command, parserInfo, run, runCommand) where

import Control.Applicative (Alternative(many))
import Data.Traversable (Traversable)
import PGT
import PGT.Prelude
import PGT.Selector
import System.IO (IO)

import qualified Options.Applicative as CLI
import qualified System.IO           as IO
import qualified System.Path         as Path

data Command = forall t . (Traversable t) => Command (Config -> [Test] -> IO ()) (t Selector)

run :: forall m . MonadIO m => [String] -> m ()
run arguments = liftIO $ do
  IO.hSetBuffering IO.stdout IO.LineBuffering
  command <- CLI.handleParseResult $
    CLI.execParserPure CLI.defaultPrefs parserInfo arguments
  runCommand command =<< fromEnv

runCommand :: MonadIO m => Command -> Config -> m ()
runCommand (Command action selectors) config = liftIO $ action config =<< expand selectors

parserInfo :: CLI.ParserInfo Command
parserInfo = wrapHelper subcommands
  where
    selector  = CLI.argument (Selector <$> CLI.eitherReader Path.parse) (CLI.metavar "SELECTOR")
    selectors = many selector

    wrapHelper :: CLI.Parser a -> CLI.ParserInfo a
    wrapHelper parser = CLI.info (CLI.helper <*> parser) CLI.idm

    subcommands =
      CLI.subparser
        $  mkCommand "list"   runList
        <> mkCommand "run"    runExamples
        <> mkCommand "test"   runTests
        <> mkCommand "update" runUpdates

    mkCommand name action =
      CLI.command name $ wrapHelper (Command action <$> selectors)

