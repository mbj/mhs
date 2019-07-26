module OpenApi.Paths where

import Control.Applicative ((*>), (<*))
import Data.Foldable (any)
import Data.Functor (($>))
import GHC.Generics (Generic)
import OpenApi.JSON
import OpenApi.Prelude

import qualified Data.Aeson           as JSON
import qualified Data.Aeson.Types     as JSON
import qualified Data.Attoparsec.Text as Text
import qualified Data.Char            as Char
import qualified Data.Map.Strict      as Map
import qualified GHC.Enum             as GHC

data Operation = Operation
  { description :: Maybe OperationDescription
  , operationId :: OperationId
  , parameters  :: Maybe [Parameter]
  }
  deriving anyclass JSON.FromJSON
  deriving stock    (Generic, Show)

newtype OperationDescription = OperationDescription Text
  deriving newtype JSON.FromJSON
  deriving stock   Show

newtype OperationId = OperationId Text
  deriving newtype JSON.FromJSON
  deriving stock   Show

data Parameter = Parameter
  { description :: Maybe ParameterDescription
  , location    :: ParameterLocation
  , name        :: ParameterName
  , required    :: Bool
  , style       :: ParameterStyle
  }
  deriving stock (Generic, Show)

instance JSON.FromJSON Parameter where
  parseJSON = parseRenamed $ Map.singleton "location" "in"

newtype ParameterDescription = ParameterDescription Text
  deriving newtype JSON.FromJSON
  deriving stock   Show

data ParameterLocation = Cookie | Header | Path | Query
  deriving stock (GHC.Bounded, GHC.Enum, Show)

instance JSON.FromJSON ParameterLocation where
  parseJSON = parseJSONFixed "ParameterLocation" JSON.withText $ \case
    Cookie -> "cookie"
    Header -> "header"
    Path   -> "path"
    Query  -> "query"

newtype ParameterName = ParameterName Text
  deriving newtype JSON.FromJSON
  deriving stock   Show

data ParameterStyle = DeepObject | Form | Simple
  deriving stock (GHC.Bounded, GHC.Enum, Show)

instance JSON.FromJSON ParameterStyle where
  parseJSON = parseJSONFixed "ParameterStyle" JSON.withText $ \case
    DeepObject -> "deepObject"
    Form       -> "form"
    Simple     -> "simple"

data Segment = Static Text | Dynamic Text
  deriving stock (Eq, Ord, Show)

newtype Template = Template [Segment]
  deriving newtype (Eq, Ord)
  deriving stock   Show

instance JSON.FromJSON Template where
  parseJSON = JSON.withText "path item" parseTemplateText

instance JSON.FromJSONKey Template where
   fromJSONKey = JSON.FromJSONKeyTextParser parseTemplateText

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

    dynamicSegment   = skip '{' *> (Dynamic <$> segmentName) <* skip '}'
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
  deriving anyclass JSON.FromJSON
  deriving stock    (Generic, Show)
