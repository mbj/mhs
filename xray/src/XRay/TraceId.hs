module XRay.TraceId (TraceId(..), newTraceIdIO, traceIdParser) where

import Control.Applicative ((*>))
import Data.Aeson.Types (ToJSON, toJSON)
import Data.ByteString.Builder (toLazyByteString, word32HexFixed)
import Data.ByteString.Lazy (toStrict)
import Data.Text.Encoding (decodeUtf8)
import Data.Time.Clock.System (SystemTime(systemSeconds), getSystemTime)
import Data.Word (Word32)
import GHC.Real (fromIntegral)
import System.Random (randomIO)
import XRay.Parser
import XRay.Prelude

import qualified Data.Attoparsec.Text  as Text

-- | Trace ID segments can be associated with.
--
-- Trace ID Format
--
-- A trace_id consists of three numbers separated by hyphens.
-- For example, 1-58406520-a006649127e371903a2de979. This includes:
--
-- * The version number, that is, 1.
-- * The time of the original request, in Unix epoch time, in 8 hexadecimal digits.
--   For example, 10:00AM December 1st, 2016 PST in epoch time is 1480615200
--   seconds, or 58406520 in hexadecimal digits.
-- * A 96-bit identifier for the trace, globally unique, in 24 hexadecimal digits.
data TraceId = TraceId
  { requestTime :: Word32                    -- ^ Original request time, 4 bytes
  , uniqueId    :: (Word32, Word32, Word32)  -- ^ Globally unique ID, 12 bytes
  }
  deriving stock (Eq, Show)

instance Conversion Text TraceId where
  convert (TraceId requestTime (wordA, wordB, wordC)) =
    decodeUtf8 . toStrict $ toLazyByteString builder
    where
      builder
        =  "1-"
        <> word32HexFixed requestTime
        <> "-"
        <> word32HexFixed wordA
        <> word32HexFixed wordB
        <> word32HexFixed wordC

instance ToJSON TraceId where
  toJSON = toJSON . toText

traceIdParser :: Text.Parser TraceId
traceIdParser =
  "1-" *> (TraceId <$> requestTime <*> ("-" *> unique))

  where
    requestTime = word32

    unique :: Text.Parser (Word32, Word32, Word32)
    unique = (,,) <$> word32 <*> word32 <*> word32

    word32 :: Text.Parser Word32
    word32 = fixedHex 8

newTraceIdIO :: IO TraceId
newTraceIdIO = do
  requestTime <- fromIntegral . systemSeconds <$> getSystemTime
  uniqueId    <- (,,) <$> randomIO <*> randomIO <*> randomIO

  pure $ TraceId {..}
