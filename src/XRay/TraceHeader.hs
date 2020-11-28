module XRay.TraceHeader
  ( TraceHeader(..)
  , TraceHeaderFormatError(..)
  , parseTraceHeader
  )
where

import Control.Applicative ((*>), (<*), liftA2)
import Data.Attoparsec.Text ((<?>))
import XRay.Prelude
import XRay.Segment
import XRay.TraceId

import qualified Control.Exception    as Control
import qualified Data.Attoparsec.Text as Text
import qualified Data.Char            as Char
import qualified Data.List            as List
import qualified Data.Text            as Text

data TraceHeader = TraceHeader
  { parentId  :: Maybe SegmentId
  , traceId   :: TraceId
  , sampled   :: Maybe Bool
  }
  deriving stock (Eq, Show)

newtype TraceHeaderFormatError = TraceHeaderFormatError Text
  deriving stock (Eq, Show)

instance Control.Exception TraceHeaderFormatError

parseTraceHeader :: Text -> Either TraceHeaderFormatError TraceHeader
parseTraceHeader
  = left TraceHeaderFormatError
  . parseAll (traceHeaderParser <* Text.endOfInput)

{-

There is no good AWS resource on details of tracing headers.

The best one found so far is this one:

https://docs.aws.amazon.com/xray/latest/devguide/xray-concepts.html#xray-concepts-tracingheader

But there are many open questions, whichh mostly have to be answered by observation.

The parser we have in this library chose:

* On duplicate fields:
  * Select the first occurence
  * Do not even validate the value of later occurencies.
* Reject trailing ';'
* Allow fields to occur in any order
  (order changes where observed between different AWS services)
* Do allow any capitalization of field names.
-}

traceHeaderParser :: Text.Parser TraceHeader
traceHeaderParser =
  fromFields =<< Text.sepBy ((,) <$> key <*> value) (Text.char ';')
  where
    key :: Text.Parser Text
    key = Text.toLower <$> Text.takeWhile1 (liftA2 (||) Char.isLower Char.isUpper)

    value :: Text.Parser Text
    value = "=" *> Text.takeWhile1 (';' /=)

    boolParser :: Text.Parser Bool
    boolParser = Text.anyChar >>= \case
      '0'    -> pure False
      '1'    -> pure True
      _other -> fail "Unexpected sampled value"

    fromFields :: [(Text, Text)] -> Text.Parser TraceHeader
    fromFields fields = do
      traceId  <- required "Missing root field" $ parseField "root" traceIdParser
      parentId <- parseField "parent"  segmentIdParser
      sampled  <- parseField "sampled" boolParser
      pure $ TraceHeader{..}
      where
        parseField :: Text -> Text.Parser a -> Text.Parser (Maybe a)
        parseField field parser
          = maybe (pure empty) (fmap pure . runParser (parser <?> convert field))
          $ List.lookup field fields

        required :: Text -> Text.Parser (Maybe a) -> Text.Parser a
        required message parser = maybe (fail $ convert message) pure =<< parser

parseAll :: Text.Parser a -> Text -> Either Text a
parseAll parser = left convert . Text.parseOnly (parser <* Text.endOfInput)

runParser :: Text.Parser a -> Text -> Text.Parser a
runParser parser = either (fail . convert) pure . parseAll parser
