module LHT.Zip (mkZip) where

import Data.Bits (shiftL)
import Data.Foldable (foldr')
import GHC.Real (fromIntegral, toInteger)
import LHT.Prelude
import System.Posix.Types (FileMode)

import qualified Codec.Archive.Zip    as Zip
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import qualified System.Path          as Path
import qualified System.Posix.Files   as File

mkZip :: [(Path.RelFile, BS.ByteString)] -> BS.ByteString -> LBS.ByteString
mkZip extra bootstrap = Zip.fromArchive archive
  where
    archive =
      foldr'
        (Zip.addEntryToArchive . mkEntry)
        (Zip.addEntryToArchive bootstrapEntry Zip.emptyArchive)
        extra

    mkEntry :: (Path.RelFile, BS.ByteString) -> Zip.Entry
    mkEntry (name, content) =
      setMode
        File.otherReadMode
        (Zip.toEntry (Path.toString name) 0 $ LBS.fromStrict content)

    bootstrapEntry =
      setMode
        (File.unionFileModes File.otherExecuteMode File.otherReadMode)
        (Zip.toEntry "bootstrap" 0 $ LBS.fromStrict bootstrap)

    setMode :: FileMode -> Zip.Entry -> Zip.Entry
    setMode newMode entry = entry
      { Zip.eExternalFileAttributes = fromIntegral (shiftL (toInteger newMode) 16)
      , Zip.eVersionMadeBy          = 0x0300  -- UNIX file attributes
      }
