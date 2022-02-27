module OpenApi.Responses where

import Control.Applicative ((<*))
import Data.Aeson ((.:?))
import Data.Maybe (catMaybes)
import OpenApi.HTTP
import OpenApi.Prelude
import OpenApi.ReferenceOr
import OpenApi.Response

import qualified Data.Aeson                as JSON
import qualified Data.Aeson.Key            as JSON.Key
import qualified Data.Aeson.KeyMap         as JSON.KeyMap
import qualified Data.Aeson.Types          as JSON
import qualified Data.Attoparsec.Text      as Text
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
    patterns <- parsePatterns . JSON.KeyMap.toList $ JSON.KeyMap.delete "default" object
    pure Responses{..}
    where
      parsePatterns
        :: [(JSON.Key, JSON.Value)]
        -> JSON.Parser (Map ResponseStatusPattern (ReferenceOr Response))
      parsePatterns pairs = Map.fromList <$> traverse (uncurry parsePair) pairs

      parsePair
        :: JSON.Key
        -> JSON.Value
        -> JSON.Parser (ResponseStatusPattern, ReferenceOr Response)
      parsePair key value =
        (,) <$> parseResponseStatusPattern (JSON.Key.toText key) <*> JSON.parseJSON value

      parseResponseStatusPattern :: Text -> JSON.Parser ResponseStatusPattern
      parseResponseStatusPattern input =
        either
          (const . fail $ "Invalid status code pattern: " <> show input)
          pure $ Text.parseOnly statusCodeParser input

      statusCodeParser =
        maybe (fail "Invalid status code") (pure . ResponseStatusExact)
          =<< (mkStatus <$> Text.decimal) <* Text.endOfInput

instance JSON.ToJSON Responses where
  toJSON Responses{..} = JSON.Object . JSON.KeyMap.fromList $ catMaybes pairs
    where
      pairs :: [Maybe (JSON.Key, JSON.Value)]
      pairs
        = (("default",) . JSON.toJSON <$> default')
        : (pure <$> responsePairs)

      responsePairs :: [(JSON.Key, JSON.Value)]
      responsePairs = uncurry mkPattern <$> Map.toList patterns

      mkPattern :: ResponseStatusPattern -> ReferenceOr Response -> (JSON.Key, JSON.Value)
      mkPattern (ResponseStatusExact httpStatus) =
        (JSON.Key.fromText . convertText . show $ HTTP.statusCode httpStatus,) . JSON.toJSON

newtype ResponseStatusPattern = ResponseStatusExact HTTP.Status
  deriving stock (Eq, Ord, Show)

instance Conversion Text ResponseStatusPattern where
  convert = convertText . show
