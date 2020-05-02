module OpenApi.Components where

import OpenApi.JSON
import OpenApi.Parameter
import OpenApi.Prelude
import OpenApi.Reference
import OpenApi.Response
import OpenApi.Schema
import OpenApi.SecurityScheme
import OpenApi.TaggedText

import qualified Data.Aeson as JSON

data Components = Components
  { parameters      :: Maybe (Map (TaggedText "ParameterKey")      (ReferenceOr Parameter))
  , responses       :: Maybe (Map (TaggedText "ResponseKey")       (ReferenceOr Response))
  , schemas         :: Maybe (Map (TaggedText "SchemaKey")         (ReferenceOr Schema))
  , securitySchemes :: Maybe (Map (TaggedText "SecuritySchemeKey") (ReferenceOr SecurityScheme))
  }
  deriving stock (Eq, Generic, Show)

instance JSON.FromJSON Components where
  parseJSON = genericParseJSON
