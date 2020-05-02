module OpenApi.Reference where

import OpenApi.Prelude

import qualified Data.Aeson          as JSON
import qualified Data.Aeson.Types    as JSON
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text           as Text

class Referencable a where
  targetName    :: String
  referencePath :: [Text]

renderReference :: [Text] -> Text
renderReference = ("#/" <>) . Text.intercalate "/"

newtype Reference a = Reference [Text]
  deriving stock (Eq, Show)

instance Referencable a => JSON.ToJSON (Reference a) where
  toJSON (Reference path) = JSON.toJSON $ renderReference path

instance Referencable a => JSON.FromJSON (Reference a) where
  parseJSON = JSON.withText (targetName @a <> "reference") $ \value ->
    if expectedPrefix `Text.isPrefixOf` value
      then pure $ Reference $ referencePath @a <> [Text.drop (Text.length expectedPrefix) value]
      else fail $ "Cannot parse reference to " <> targetName @a <> " with value " <> convertText value
   where
     expectedPrefix = renderReference (referencePath @a) <> "/"

data ReferenceOr a = ReferenceTo (Reference a) | Literal a
  deriving stock (Eq, Show)

instance (Referencable a, JSON.FromJSON a) => JSON.FromJSON (ReferenceOr a) where
  parseJSON input = JSON.withObject (targetName @a) parseObject input
    where
      parseObject :: JSON.Object -> JSON.Parser (ReferenceOr a)
      parseObject object
        = maybe (Literal <$> JSON.parseJSON input) (parseReference object)
        $ HashMap.lookup "$ref" object

      parseReference :: JSON.Object -> JSON.Value -> JSON.Parser (ReferenceOr a)
      parseReference object value =
        if HashMap.size object == 1
          then ReferenceTo <$> JSON.parseJSON value
          else fail "$ref key with siblings"

instance (Referencable a, JSON.ToJSON a) => JSON.ToJSON (ReferenceOr a) where
  toJSON = \case
    Literal value     -> JSON.toJSON value
    ReferenceTo value -> JSON.toJSON value
