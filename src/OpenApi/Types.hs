module OpenApi.Types where

import Data.Map (Map)
import GHC.Generics (Generic)
import OpenApi.JSON
import OpenApi.Prelude
import OpenApi.TaggedText

import qualified Data.Aeson      as JSON
import qualified Data.Map.Strict as Map
import qualified GHC.Enum        as GHC
import qualified OpenApi.Paths   as Paths
import qualified OpenApi.Schema  as Schema

data SecuritySchemeType = HTTP
  deriving stock (Eq, GHC.Bounded, GHC.Enum, Show)

instance JSON.FromJSON SecuritySchemeType where
  parseJSON = parseJSONFixed "SecuritySchemeType" JSON.withText $ \case
    HTTP -> "http"

data SecuritySchemeScheme = Basic | Bearer
  deriving stock (Eq, GHC.Bounded, GHC.Enum, Show)

instance JSON.FromJSON SecuritySchemeScheme where
  parseJSON = parseJSONFixed "SecuritySchemeScheme" JSON.withText $ \case
    Basic  -> "basic"
    Bearer -> "bearer"

data SecurityScheme = SecurityScheme
  { scheme :: SecuritySchemeScheme
  , type'  :: SecuritySchemeType
  }
  deriving stock (Eq, Generic, Show)

instance JSON.FromJSON SecurityScheme where
  parseJSON = parseRenamed $ Map.singleton "type'" "type"

data Components = Components
  { schemas         :: Map Schema.ReferenceName Schema.SchemaObject
  , securitySchemes :: Map (TaggedText "SecurityScheme" SecurityScheme) SecurityScheme
  }
  deriving stock (Eq, Generic, Show)

instance JSON.FromJSON Components where
  parseJSON = genericParseJSON

data Specification = Specification
  { components :: Components
  , paths      :: Map Paths.Template Paths.Item
  }
  deriving stock (Eq, Generic, Show)

instance JSON.FromJSON Specification where
  parseJSON = genericParseJSON
