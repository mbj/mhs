module OpenApi.Reference where

import OpenApi.Prelude
import OpenApi.Referencable

import qualified Data.Aeson as JSON
import qualified Data.Text  as Text

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
