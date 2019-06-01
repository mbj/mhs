{-# LANGUAGE ExistentialQuantification #-}

module PGT.CLI (run) where

import Control.Applicative (Alternative(many))
import Control.Monad (mapM)
import Data.Foldable (Foldable)
import Data.Set.Ordered (OSet, (|<>))
import Data.String (String)
import Data.Traversable (Traversable)
import PGT
import PGT.Prelude
import System.FilePath ((</>))
import System.IO (IO)

import qualified Data.Foldable             as Foldable
import qualified Data.List                 as List
import qualified Data.Set.Ordered          as OSet
import qualified Options.Applicative       as CLI
import qualified Options.Applicative.Types as CLI
import qualified System.Directory          as FS
import qualified System.FilePath           as Path
import qualified System.IO                 as IO
import qualified System.Posix.Files        as FS

newtype Selector = Selector Path.FilePath

data Command    = Command Options Subcommand
data Subcommand = forall t . (Traversable t) => Subcommand (Config -> [Test] -> IO ()) (t Selector)

run :: forall f m . (Foldable f, MonadIO m) => f String -> m ()
run arguments = do
  setupStdoutBuffer
  runCommand =<< parseCLI arguments
  where
    setupStdoutBuffer :: m ()
    setupStdoutBuffer = liftIO $ IO.hSetBuffering IO.stdout IO.LineBuffering

    runCommand :: Command -> m ()
    runCommand (Command options (Subcommand action selectors)) = do
      config <- fromEnv options
      liftIO $ action config =<< (makeTests <$> expand selectors)

makeTests :: Foldable f => f String -> [Test]
makeTests files = (\(id, path) -> Test{..}) <$> List.zip [0..] (Foldable.toList files)

parseCLI :: (Foldable f, MonadIO m) => f String -> m Command
parseCLI
  = liftIO
  . CLI.handleParseResult
  . CLI.execParserPure CLI.defaultPrefs main
  . Foldable.toList
  where
    failFastFlag  = CLI.flag Continue Stop $ CLI.long "fail-fast" <> CLI.help "Stop on first error"
    main          = wrapHelper mainCommand
    mainCommand   = Command <$> optionsParser <*> subcommands
    optionsParser = Options <$> failFastFlag <*> outputFlag
    outputFlag    = CLI.flag Verbose Silent $ CLI.long "silent" <> CLI.help "Silence all output"
    selector      = CLI.argument (Selector <$> CLI.readerAsk) (CLI.metavar "SELECTOR")
    selectors     = many selector

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

expand
  :: (MonadFail m, MonadIO m, Traversable t)
  => t Selector
  -> m (OSet Path.FilePath)
expand = (flatten <$>) . mapM expandSelector

expandSelector
  :: forall m . (MonadFail m, MonadIO m)
  => Selector
  -> m (OSet Path.FilePath)
expandSelector (Selector path) = expandStatus =<< liftIO (FS.getFileStatus path)
  where
    expandStatus :: FS.FileStatus -> m (OSet Path.FilePath)
    expandStatus status
      | FS.isRegularFile status = pure $ OSet.singleton path
      | FS.isDirectory status   = expandDirectory path
      | otherwise            = fail $ "Path: " <> path <> " is not a file or directory"

expandDirectory
  :: forall m . MonadIO m
  => Path.FilePath
  -> m (OSet Path.FilePath)
expandDirectory path =
  flatten <$> (mapM expandEntry =<< (List.sort <$> liftIO (FS.listDirectory path)))
  where
    expandEntry entry =
      let candidate = path </> entry
        in expandStatus candidate =<< liftIO (FS.getFileStatus candidate)

    expandStatus :: Path.FilePath -> FS.FileStatus -> m (OSet Path.FilePath)
    expandStatus candidate status
      | FS.isRegularFile status = pure $ expandRegular candidate
      | FS.isDirectory status   = expandDirectory candidate
      | otherwise            = pure OSet.empty

    expandRegular candidate =
      if Path.takeExtension candidate == ".sql"
         then OSet.singleton candidate
         else OSet.empty

flatten :: (Foldable f, Ord a) => f (OSet a) -> OSet a
flatten = Foldable.foldl' (|<>) OSet.empty
