{-# LANGUAGE ExistentialQuantification #-}

module PGT.Selector (Selector(..), expand) where

import Data.Set.Ordered (OSet, (|<>))
import Data.Traversable (Traversable)
import PGT
import PGT.Prelude
import System.Path ((</>))

import qualified Data.Foldable         as Foldable
import qualified Data.List             as List
import qualified Data.Set.Ordered      as OSet
import qualified System.Path           as Path
import qualified System.Path.Directory as Path
import qualified System.Posix.Files    as FS

newtype Selector = Selector Path.RelFileDir

expand
  :: (MonadFail m, MonadIO m, Traversable t)
  => t Selector
  -> m [Test]
expand = fmap (makeTests . Foldable.toList . flatten) . traverse expandSelector
  where
    makeTests :: [Path.RelFile] -> [Test]
    makeTests files = (\(id, path) -> Test{..}) <$> List.zip [0..] files

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
