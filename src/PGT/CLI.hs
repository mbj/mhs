{-# LANGUAGE ExistentialQuantification #-}

module PGT.CLI (run) where

import Control.Applicative (Alternative(many))
import Data.Functor (fmap)
import Data.Set.Ordered (OSet, (|<>))
import Data.String (String)
import Data.Traversable (Traversable, traverse)
import PGT
import PGT.Prelude
import System.IO (IO)
import System.Path ((</>))

import qualified Data.Foldable         as Foldable
import qualified Data.List             as List
import qualified Data.Set.Ordered      as OSet
import qualified Options.Applicative   as CLI
import qualified System.IO             as IO
import qualified System.Path           as Path
import qualified System.Path.Directory as Path
import qualified System.Posix.Files    as FS

newtype Selector = Selector Path.RelFileDir

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

    makeTests :: [Path.RelFile] -> [Test]
    makeTests files = (\(id, path) -> Test{..}) <$> List.zip [0..] files

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
    selector      = CLI.argument (Selector <$> CLI.eitherReader Path.parse) (CLI.metavar "SELECTOR")
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
  -> m [Path.RelFile]
expand = fmap (Foldable.toList . flatten) . traverse expandSelector

expandSelector
  :: forall m . (MonadFail m, MonadIO m)
  => Selector
  -> m (OSet Path.RelFile)
expandSelector (Selector path) = expandStatus =<< liftIO (FS.getFileStatus stringPath)
  where
    expandStatus :: FS.FileStatus -> m (OSet Path.RelFile)
    expandStatus status
      | FS.isRegularFile status = pure . OSet.singleton $ Path.relFile stringPath
      | FS.isDirectory status   = expandDirectory $ Path.relDir stringPath
      | otherwise               = fail $ "Path: " <> stringPath <> " is not a regular file or directory"

    stringPath = Path.toString path

expandDirectory
  :: forall m . MonadIO m
  => Path.RelDir
  -> m (OSet Path.RelFile)
expandDirectory directory =
  flatten <$> (traverse expandCandidate =<< getCandidates)
  where
    getCandidates :: m [Path.RelFileDir]
    getCandidates
      = List.sort
      . fmap (directory </>) <$> liftIO (Path.getDirectoryContents directory)

    expandCandidate :: Path.RelFileDir -> m (OSet Path.RelFile)
    expandCandidate candidate =
      expandStatus candidate =<< liftIO (FS.getFileStatus $ Path.toString candidate)

    expandStatus :: Path.RelFileDir -> FS.FileStatus -> m (OSet Path.RelFile)
    expandStatus candidate status
      | FS.isRegularFile status = pure . expandRegular $ Path.relFile stringCandidate
      | FS.isDirectory status   = expandDirectory $ Path.relDir stringCandidate
      | otherwise               = pure OSet.empty
      where
        stringCandidate = Path.toString candidate

    expandRegular :: Path.RelFile -> OSet Path.RelFile
    expandRegular candidate =
      if Path.takeExtension candidate == ".sql"
         then OSet.singleton candidate
         else OSet.empty

flatten :: (Foldable f, Ord a) => f (OSet a) -> OSet a
flatten = Foldable.foldl' (|<>) OSet.empty
