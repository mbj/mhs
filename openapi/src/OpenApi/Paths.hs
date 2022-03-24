module OpenApi.Paths where

import Control.Applicative ((*>), (<*))
import Data.Char (Char)
import Data.Foldable (any)
import OpenApi.Parameter
import OpenApi.PathItem
import OpenApi.Prelude
import OpenApi.TaggedText

import qualified Data.Aeson           as JSON
import qualified Data.Aeson.Types     as JSON
import qualified Data.Attoparsec.Text as Text
import qualified Data.Char            as Char
import qualified Data.Text            as Text

type Paths = Map PathTemplate PathItem

data PathSegment = PathSegmentStatic Text | PathSegmentDynamic ParameterName
  deriving stock (Eq, Ord, Show)

instance Conversion Text PathSegment where
  convert = \case
    PathSegmentDynamic parameterName -> "{" <> convertText parameterName <> "}"
    PathSegmentStatic text -> text

newtype PathTemplate = PathTemplate [PathSegment]
  deriving stock (Eq, Ord, Show)

instance Conversion Text PathTemplate where
  convert (PathTemplate segments) = Text.intercalate "" $ toText <$> segments

instance JSON.FromJSON PathTemplate where
  parseJSON = JSON.withText "path template" parsePathTemplateText

instance JSON.FromJSONKey PathTemplate where
  fromJSONKey = JSON.FromJSONKeyTextParser parsePathTemplateText

instance JSON.ToJSON PathTemplate where
  toJSON = JSON.toJSON . convert @Text

parsePathTemplateText :: Text -> JSON.Parser PathTemplate
parsePathTemplateText input
  = either
    (const . fail $ "invalid template path: " <> show input)
    pure
  $ Text.parseOnly (parser <* Text.endOfInput) input
  where
    parser :: Text.Parser PathTemplate
    parser = PathTemplate <$> Text.many' anyPathSegment

    anyPathSegment :: Text.Parser PathSegment
    anyPathSegment = dynamicPathSegment <|> (PathSegmentStatic <$> staticPart)

    dynamicPathSegment = expect '{' *> (PathSegmentDynamic . TaggedText <$> segmentName) <* expect '}'

    segmentChar char = any ($ char) ([Char.isDigit, Char.isLower, Char.isUpper, (== '_'), (== '-')] :: [Char -> Bool])

    staticPart = Text.takeWhile1 notControl

    notControl = \case
      '{' -> False
      '}' -> False
      _   -> True

    segmentName = Text.takeWhile1 segmentChar
    expect      = void . Text.char
