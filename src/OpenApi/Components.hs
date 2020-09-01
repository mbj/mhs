module OpenApi.Components where

import OpenApi.JSON
import OpenApi.Parameter
import OpenApi.Prelude
import OpenApi.Reference
import OpenApi.ReferenceOr
import OpenApi.RequestBody
import OpenApi.Response
import OpenApi.Schema
import OpenApi.SecurityScheme

import qualified Data.Aeson as JSON

newtype ComponentName a = ComponentName Text
  deriving (Conversion Text) via Text
  deriving newtype
    ( JSON.FromJSON
    , JSON.FromJSONKey
    , JSON.ToJSON
    , JSON.ToJSONKey
    )
  deriving stock (Eq, Ord, Show)

fromReference :: Reference a -> ComponentName a
fromReference = ComponentName . toText

type Parameters      = Map (ComponentName Parameter)      (ReferenceOr Parameter)
type RequestBodies   = Map (ComponentName RequestBody)    (ReferenceOr RequestBody)
type Responses       = Map (ComponentName Response)       (ReferenceOr Response)
type Schemas         = Map (ComponentName Schema)         (ReferenceOr Schema)
type SecuritySchemes = Map (ComponentName SecurityScheme) (ReferenceOr SecurityScheme)

data Components = Components
  { parameters      :: Maybe Parameters
  , requestBodies   :: Maybe RequestBodies
  , responses       :: Maybe Responses
  , schemas         :: Maybe Schemas
  , securitySchemes :: Maybe SecuritySchemes
  }
  deriving stock (Eq, Generic, Show)

instance JSON.FromJSON Components where
  parseJSON = genericParseJSON
