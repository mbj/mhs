module CBT.Backend.Tar (findEntry) where

import CBT.Prelude

import qualified Codec.Archive.Tar       as Tar
import qualified Data.ByteString.Lazy    as LBS
import qualified System.Path             as Path

findEntry :: LBS.ByteString -> Path.RelFile -> Maybe LBS.ByteString
findEntry input path =
  Tar.foldEntries
    (\current prev -> candidateEntry current <|> prev)
    empty
    (const empty)
    (Tar.read input)
  where
    candidateEntry :: Tar.Entry -> Maybe LBS.ByteString
    candidateEntry entry =
      if Tar.entryPath entry == Path.toString path
        then candidateContent $ Tar.entryContent entry
        else empty

    candidateContent :: Tar.EntryContent -> Maybe LBS.ByteString
    candidateContent = \case
      (Tar.NormalFile content _size) -> pure content
      _other                         -> empty
