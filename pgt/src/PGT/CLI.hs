{-# LANGUAGE ApplicativeDo             #-}
{-# LANGUAGE ExistentialQuantification #-}

module PGT.CLI (Command, parserInfo, parserInfoCommand, run, runCommand) where

import Control.Applicative (Alternative(many))
import PGT
import PGT.Prelude
import PGT.Selector

import qualified Data.Vector         as Vector
import qualified Options.Applicative as CLI
import qualified System.Exit         as System
import qualified System.IO           as IO
import qualified System.Path         as Path

data Command = Command (Config -> Tests -> IO System.ExitCode) ShardCount ShardIndex [Selector]

run :: forall m . MonadIO m => [String] -> m System.ExitCode
run arguments = liftIO $ do
  IO.hSetBuffering IO.stdout IO.LineBuffering
  command <- CLI.handleParseResult $
    CLI.execParserPure CLI.defaultPrefs parserInfoCommand arguments
  runCommand command =<< fromEnv

runCommand :: forall m . MonadIO m => Command -> Config -> m System.ExitCode
runCommand (Command action shardCount shardIndex selectors) config = liftIO $ do
  shardConfig <- either fail pure $ parseShardConfig shardCount shardIndex
  action config . selectShard shardConfig . Vector.fromList =<< expand selectors

parserInfo
  :: forall m . MonadIO m
  => ((Config -> m System.ExitCode)
  -> m System.ExitCode)
  -> CLI.ParserInfo (m System.ExitCode)
parserInfo withConfig = run' <$> parserInfoCommand
  where
    run' :: Command -> m System.ExitCode
    run' command = withConfig $ runCommand command

parserInfoCommand :: CLI.ParserInfo Command
parserInfoCommand = wrapHelper subcommands
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

    mkCommand
      :: String
      -> (Config -> Tests -> IO System.ExitCode)
      -> CLI.Mod CLI.CommandFields Command
    mkCommand name action =
      CLI.command name $ wrapHelper (Command action <$> shardCountOption <*> shardIndexOption <*> selectors)

shardIndexOption :: CLI.Parser ShardIndex
shardIndexOption = CLI.option (ShardIndex <$> CLI.auto)
  (CLI.long "shard-index" <> CLI.value defaultShardIndex)

shardCountOption :: CLI.Parser ShardCount
shardCountOption = CLI.option read
  (CLI.long "shard-count" <> CLI.value defaultShardCount)
  where
    read :: CLI.ReadM ShardCount
    read = either fail pure . parseShardCount =<< CLI.auto
