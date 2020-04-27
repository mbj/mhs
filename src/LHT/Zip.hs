module LHT.Zip (mkZip) where

import Data.Bits (shiftL)
import Data.ByteString (ByteString)
import Data.Foldable (foldr')
import GHC.Real (fromIntegral, toInteger)
import LHT.Prelude
import System.Posix.Types (FileMode)

import qualified Codec.Archive.Zip    as Zip
import qualified Data.ByteString.Lazy as LBS
import qualified System.Posix.Files   as File

mkZip :: ByteString -> LBS.ByteString
mkZip = Zip.fromArchive . mkFunctionArchive

mkFunctionArchive :: ByteString -> Zip.Archive
mkFunctionArchive bootstrap = Zip.addEntryToArchive bootstrapEntry Zip.emptyArchive
  where
    bootstrapEntry =
      setMode
        bootstrapFileMode
        (Zip.toEntry "bootstrap" 0 $ LBS.fromStrict bootstrap)

    bootstrapFileMode =
      foldr'
        File.unionFileModes
        File.regularFileMode
        ([File.otherExecuteMode, File.otherReadMode] :: [FileMode])

setMode :: FileMode -> Zip.Entry -> Zip.Entry
setMode newMode entry = entry
  { Zip.eExternalFileAttributes = fromIntegral (shiftL (toInteger newMode) 16)
  , Zip.eVersionMadeBy          = 0x0300  -- UNIX file attributes
  }
