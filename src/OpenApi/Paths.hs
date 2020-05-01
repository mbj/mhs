{-# LANGUAGE TupleSections #-}

module OpenApi.Paths where

import Control.Applicative ((*>), (<*))
import Data.Aeson ((.:?))
import Data.Foldable (any)
import Data.Functor (($>))
import Data.Map.Strict (Map)
import Data.Maybe (catMaybes)
import Data.String (String)
import Data.Traversable (traverse)
import Data.Tuple (uncurry)
import GHC.Generics (Generic)
import OpenApi.Description
import OpenApi.HTTP
import OpenApi.JSON
import OpenApi.Prelude
import OpenApi.Schema
import OpenApi.TaggedText

import qualified Data.Aeson                as JSON
import qualified Data.Aeson.Types          as JSON
import qualified Data.Attoparsec.Text      as Text
import qualified Data.Char                 as Char
import qualified Data.HashMap.Strict       as HashMap
import qualified Data.Map.Strict           as Map
import qualified Data.Text                 as Text
import qualified GHC.Enum                  as GHC
import qualified Network.HTTP.Types.Status as HTTP

data Operation = Operation
  { deprecated  :: Maybe Bool
  , description :: Maybe (Description Operation)
  , operationId :: TaggedText "OperationId" ()
  , parameters  :: Maybe [Parameter]
  , requestBody :: Maybe RequestBody
  , responses   :: Responses
  }
  deriving anyclass JSON.ToJSON
  deriving stock    (Eq, Generic, Show)

instance JSON.FromJSON Operation where
  parseJSON = genericParseJSON

instance HasDescription Operation where
  getDescription = description

data Responses = Responses
  { default' :: Maybe Response
  , patterns :: Map ResponseStatusPattern Response
  }
  deriving stock (Eq, Show)

instance JSON.FromJSON Responses where
  parseJSON = JSON.withObject "responses" $ \object -> do
    default' <- object .:? "default"
    patterns <- parsePatterns . HashMap.toList $ HashMap.delete "default" object
    pure Responses{..}
    where
      parsePatterns
        :: [(Text, JSON.Value)]
        -> JSON.Parser (Map ResponseStatusPattern Response)
      parsePatterns pairs = Map.fromList <$> traverse (uncurry parsePair) pairs

      parsePair
        :: Text
        -> JSON.Value
        -> JSON.Parser (ResponseStatusPattern, Response)
      parsePair key value =
        (,) <$> parseResponseStatusPattern key <*> JSON.parseJSON value

      parseResponseStatusPattern :: Text -> JSON.Parser ResponseStatusPattern
      parseResponseStatusPattern input =
        either
          (const . fail $ "Invalid status code pattern: " <> show input)
          pure $ Text.parseOnly statusCodeParser input

      statusCodeParser =
        maybe (fail "Invalid status code") (pure . ResponseStatusExact)
          =<< (mkStatus <$> Text.decimal) <* Text.endOfInput

instance JSON.ToJSON Responses where
  toJSON Responses{..} = JSON.Object . HashMap.fromList $ catMaybes pairs
    where
      pairs :: [Maybe (Text, JSON.Value)]
      pairs
        = (("default",) . JSON.toJSON <$> default')
        : (pure <$> responsePairs)

      responsePairs :: [(Text, JSON.Value)]
      responsePairs = uncurry mkPattern <$> Map.toList patterns

      mkPattern :: ResponseStatusPattern -> Response -> (Text, JSON.Value)
      mkPattern (ResponseStatusExact httpStatus) =
        (convertText . show $ HTTP.statusCode httpStatus,) . JSON.toJSON

data Response = Response
  { content     :: Map MediaTypeQuery MediaType
  , description :: Maybe (Description Response)
  , headers     :: Maybe (Map Text ResponseHeader)
  }
  deriving anyclass JSON.ToJSON
  deriving stock    (Eq, Generic, Show)

instance JSON.FromJSON Response where
  parseJSON = genericParseJSON

instance HasDescription Response where
  getDescription = description

data ResponseHeader = ResponseHeader
  { description :: Maybe (Description ResponseHeader)
  , name        :: TaggedText "HeaderName" ()
  }
  deriving anyclass JSON.ToJSON
  deriving stock    (Eq, Generic, Show)

instance JSON.FromJSON ResponseHeader where
  parseJSON = genericParseJSON

instance HasDescription ResponseHeader where
  getDescription = description

newtype ResponseStatusPattern = ResponseStatusExact HTTP.Status
  deriving stock (Eq, Ord, Show)

data RequestBody = RequestBody
  { content     :: Map MediaTypeQuery MediaType
  , description :: Maybe (Description RequestBody)
  , required    :: Maybe Bool
  }
  deriving anyclass JSON.ToJSON
  deriving stock    (Eq, Generic, Show)

instance JSON.FromJSON RequestBody where
  parseJSON = genericParseJSON

instance HasDescription RequestBody where
  getDescription = description

type MediaTypeQuery = TaggedText "MediaTypeQuery" ()

newtype MediaType = MediaType
  { schema :: Schema }
  deriving anyclass JSON.ToJSON
  deriving stock    (Eq, Generic, Show)

instance JSON.FromJSON MediaType where
  parseJSON = genericParseJSON

data Parameter = Parameter
  { description :: Maybe (Description Parameter)
  , location    :: ParameterLocation
  , name        :: ParameterName
  , required    :: Bool
  , schema      :: Schema
  , style       :: ParameterStyle
  }
  deriving stock (Eq, Generic, Show)

instance HasDescription Parameter where
  getDescription = description

parameterRenames :: Map String String
parameterRenames = Map.singleton "location" "in"

instance JSON.FromJSON Parameter where
  parseJSON = parseRenamed parameterRenames

instance JSON.ToJSON Parameter where
  toJSON = generateRenamed parameterRenames

data ParameterLocation = Cookie | Header | Path | Query
  deriving stock (Eq, GHC.Bounded, GHC.Enum, Show)

instance JSON.FromJSON ParameterLocation where
  parseJSON = parseJSONFixed "ParameterLocation" JSON.withText toText

instance JSON.ToJSON ParameterLocation where
  toJSON = JSON.toJSON . toText

instance ToText ParameterLocation where
  toText = \case
    Cookie -> "cookie"
    Header -> "header"
    Path   -> "path"
    Query  -> "query"

type ParameterName = TaggedText "ParameterName" ()

data ParameterStyle = DeepObject | Form | Simple
  deriving stock (Eq, GHC.Bounded, GHC.Enum, Show)

instance JSON.FromJSON ParameterStyle where
  parseJSON = parseJSONFixed "ParameterStyle" JSON.withText toText

instance JSON.ToJSON ParameterStyle where
  toJSON = JSON.toJSON . toText

instance ToText ParameterStyle where
  toText = \case
    DeepObject -> "deepObject"
    Form       -> "form"
    Simple     -> "simple"

data Segment = Static Text | Dynamic ParameterName
  deriving stock (Eq, Ord, Show)

instance ToText Segment where
  toText = \case
    Dynamic parameterName -> "{" <> convertText parameterName <> "}"
    Static text -> text

newtype Template = Template [Segment]
  deriving stock (Eq, Ord, Show)

instance JSON.FromJSON Template where
  parseJSON = JSON.withText "path item" parseTemplateText

instance JSON.FromJSONKey Template where
   fromJSONKey = JSON.FromJSONKeyTextParser parseTemplateText

instance JSON.ToJSON Template where
  toJSON (Template segments)
    = JSON.toJSON . ("/" <>) . Text.intercalate "/" $ toText <$> segments

parseTemplateText :: Text -> JSON.Parser Template
parseTemplateText input =
  either
    (const . fail $ "invalid template path: " <> show input)
    pure
    $ Text.parseOnly parser input
  where
    parser :: Text.Parser Template
    parser = do
      separator

      root <|> do
        firstSegment  <- anySegment
        otherSegments <- Text.many' (separator *> anySegment)

        void Text.endOfInput

        pure . Template $ firstSegment:otherSegments

    root :: Text.Parser Template
    root = Text.endOfInput $> Template empty

    anySegment :: Text.Parser Segment
    anySegment = dynamicSegment <|> (Static <$> segmentName)

    dynamicSegment   = skip '{' *> (Dynamic . TaggedText <$> segmentName) <* skip '}'
    segmentChar char = any ($ char) [Char.isDigit, Char.isLower, Char.isUpper, (== '_')]
    segmentName      = Text.takeWhile1 segmentChar
    separator        = skip '/'
    skip             = void . Text.char

data Item = Item
  { delete  :: Maybe Operation
  , get     :: Maybe Operation
  , head    :: Maybe Operation
  , options :: Maybe Operation
  , post    :: Maybe Operation
  , put     :: Maybe Operation
  , patch   :: Maybe Operation
  }
  deriving stock (Eq, Generic, Show)

instance JSON.FromJSON Item where
  parseJSON = genericParseJSON
