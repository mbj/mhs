module OpenApi.Responses where

import Control.Applicative ((<*))
import Data.Aeson ((.:?))
import Data.Maybe (catMaybes)
import Data.Traversable (traverse)
import OpenApi.HTTP
import OpenApi.Prelude
import OpenApi.ReferenceOr
import OpenApi.Response

import qualified Data.Aeson                as JSON
import qualified Data.Aeson.Types          as JSON
import qualified Data.Attoparsec.Text      as Text
import qualified Data.HashMap.Strict       as HashMap
import qualified Data.Map.Strict           as Map
import qualified Network.HTTP.Types.Status as HTTP

data Responses = Responses
  { default' :: Maybe (ReferenceOr Response)
  , patterns :: Map ResponseStatusPattern (ReferenceOr Response)
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
        -> JSON.Parser (Map ResponseStatusPattern (ReferenceOr Response))
      parsePatterns pairs = Map.fromList <$> traverse (uncurry parsePair) pairs

      parsePair
        :: Text
        -> JSON.Value
        -> JSON.Parser (ResponseStatusPattern, ReferenceOr Response)
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

      mkPattern :: ResponseStatusPattern -> ReferenceOr Response -> (Text, JSON.Value)
      mkPattern (ResponseStatusExact httpStatus) =
        (convertText . show $ HTTP.statusCode httpStatus,) . JSON.toJSON

newtype ResponseStatusPattern = ResponseStatusExact HTTP.Status
  deriving stock (Eq, Ord, Show)

instance ToText ResponseStatusPattern where
  toText = convertText . show
