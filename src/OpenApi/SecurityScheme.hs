module OpenApi.SecurityScheme where

import OpenApi.JSON
import OpenApi.Prelude
import OpenApi.Referencable

import qualified Data.Aeson      as JSON
import qualified Data.Map.Strict as Map
import qualified GHC.Enum        as GHC

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

instance Referencable SecurityScheme where
  referencePath = ["components", "securitySchemes"]
  targetName    = "Security Scheme"

instance JSON.FromJSON SecurityScheme where
  parseJSON = parseRenamed $ Map.singleton "type'" "type"
