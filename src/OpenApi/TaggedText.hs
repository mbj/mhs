module OpenApi.TaggedText where

import OpenApi.Prelude

import qualified Data.Aeson as JSON

newtype TaggedText (label :: Symbol) = TaggedText Text
  deriving newtype
    ( JSON.FromJSON
    , JSON.FromJSONKey
    , JSON.ToJSON
    , JSON.ToJSONKey
    , ToText
    )
  deriving stock (Eq, Ord, Show)
