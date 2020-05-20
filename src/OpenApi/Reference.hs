module OpenApi.Reference where

import OpenApi.Prelude
import OpenApi.Referencable

import qualified Data.Aeson as JSON
import qualified Data.Text  as Text

newtype Reference a = Reference Text
  deriving newtype ToText
  deriving stock   (Eq, Show)

instance Referencable a => JSON.ToJSON (Reference a) where
  toJSON (Reference name) = JSON.toJSON $ prefix @a <> name

instance Referencable a => JSON.FromJSON (Reference a) where
  parseJSON = JSON.withText (targetName @a <> "reference") $ \value ->
    if (prefix @a) `Text.isPrefixOf` value
      then pure $ Reference $ Text.drop (Text.length (prefix @a)) value
      else fail $ "Cannot parse reference to " <> targetName @a <> " with value " <> convertText value

prefix :: forall a . Referencable a => Text
prefix = renderReference (referencePath @a) <> "/"
  where
    renderReference :: [Text] -> Text
    renderReference = ("#/" <>) . Text.intercalate "/"

