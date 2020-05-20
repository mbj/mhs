module OpenApi.Components where

import OpenApi.JSON
import OpenApi.Parameter
import OpenApi.Prelude
import OpenApi.ReferenceOr
import OpenApi.Response
import OpenApi.Schema
import OpenApi.SecurityScheme
import OpenApi.TaggedText

import qualified Data.Aeson as JSON

type Parameters      = Map (TaggedText "ParameterKey")      (ReferenceOr Parameter)
type Responses       = Map (TaggedText "ResponseKey")       (ReferenceOr Response)
type Schemas         = Map (TaggedText "SchemaKey")         (ReferenceOr Schema)
type SecuritySchemes = Map (TaggedText "SecuritySchemeKey") (ReferenceOr SecurityScheme)

data Components = Components
  { parameters      :: Maybe Parameters
  , responses       :: Maybe Responses
  , schemas         :: Maybe Schemas
  , securitySchemes :: Maybe SecuritySchemes
  }
  deriving stock (Eq, Generic, Show)

instance JSON.FromJSON Components where
  parseJSON = genericParseJSON
