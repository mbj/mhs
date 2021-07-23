module OpenApi.Tag where

import OpenApi.JSON
import OpenApi.Prelude
import OpenApi.TaggedText

import qualified Data.Aeson as JSON

type TagName = TaggedText "TagName"

data Tag = Tag
  { name        :: TagName
  , description :: Maybe (TaggedText "TagDescription")
  }
  deriving stock (Eq, Generic, Show)

instance JSON.FromJSON Tag where
  parseJSON = genericParseJSON

instance JSON.ToJSON Tag where
  toJSON = genericToJSON
