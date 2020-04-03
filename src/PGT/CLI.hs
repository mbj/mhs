{-# LANGUAGE ExistentialQuantification #-}

module PGT.CLI (run) where

import Control.Applicative (Alternative(many))
import Data.String (String)
import Data.Traversable (Traversable)
import PGT
import PGT.Prelude
import PGT.Selector
import System.IO (IO)

import qualified Data.Foldable       as Foldable
import qualified Options.Applicative as CLI
import qualified System.IO           as IO
import qualified System.Path         as Path

data Subcommand = forall t . (Traversable t) => Subcommand (Config -> [Test] -> IO ()) (t Selector)

run :: forall f m . (Foldable f, MonadIO m) => f String -> m ()
run arguments = do
  setupStdoutBuffer
  runSubcommand =<< parseCLI arguments
  where
    setupStdoutBuffer :: m ()
    setupStdoutBuffer = liftIO $ IO.hSetBuffering IO.stdout IO.LineBuffering

    runSubcommand :: Subcommand -> m ()
    runSubcommand (Subcommand action selectors) = do
      config <- fromEnv
      liftIO $ action config =<< expand selectors

parseCLI :: (Foldable f, MonadIO m) => f String -> m Subcommand
parseCLI
  = liftIO
  . CLI.handleParseResult
  . CLI.execParserPure CLI.defaultPrefs (wrapHelper subcommands)
  . Foldable.toList
  where
    selector  = CLI.argument (Selector <$> CLI.eitherReader Path.parse) (CLI.metavar "SELECTOR")
    selectors = many selector

    wrapHelper :: CLI.Parser a -> CLI.ParserInfo a
    wrapHelper parser = CLI.info (CLI.helper <*> parser) CLI.idm

    subcommands =
      CLI.subparser
        $  mkSubcommand "list"   runList
        <> mkSubcommand "run"    runExamples
        <> mkSubcommand "test"   runTests
        <> mkSubcommand "update" runUpdates

    mkSubcommand name action =
      CLI.command name $ wrapHelper (Subcommand action <$> selectors)
