module OpenApi.Types where

import Data.Map (Map)
import GHC.Generics (Generic)
import OpenApi.JSON
import OpenApi.Prelude

import qualified Data.Aeson      as JSON
import qualified Data.Map.Strict as Map
import qualified GHC.Enum        as GHC
import qualified OpenApi.Paths   as Paths
import qualified OpenApi.Schema  as Schema

newtype SecuritySchemeName = SecuritySchemeName Text
  deriving newtype (JSON.FromJSONKey, ToText)
  deriving stock   (Eq, Ord, Show)

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
  { schemas         :: Map Schema.Name Schema.SchemaObject
  , securitySchemes :: Map SecuritySchemeName SecurityScheme
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
