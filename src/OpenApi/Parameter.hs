module OpenApi.Parameter where

import OpenApi.JSON
import OpenApi.Prelude
import OpenApi.Reference
import OpenApi.Schema
import OpenApi.TaggedText

import qualified Data.Aeson      as JSON
import qualified Data.Map.Strict as Map
import qualified GHC.Enum        as GHC

data Parameter = Parameter
  { description :: Maybe (TaggedText "ParameterDescription")
  , location    :: ParameterLocation
  , name        :: ParameterName
  , required    :: Maybe Bool
  , schema      :: Maybe (ReferenceOr Schema)
  , style       :: Maybe ParameterStyle
  }
  deriving stock (Eq, Generic, Show)

instance Referencable Parameter where
  targetName    = "Parameter"
  referencePath = ["components", "parameters"]

parameterRenames :: Map String String
parameterRenames = Map.singleton "location" "in"

instance JSON.FromJSON Parameter where
  parseJSON = parseRenamed parameterRenames

instance JSON.ToJSON Parameter where
  toJSON = generateRenamed parameterRenames

data ParameterLocation = Cookie | Header | Path | Query
  deriving stock (Eq, GHC.Bounded, GHC.Enum, Show)

instance JSON.FromJSON ParameterLocation where
  parseJSON = parseJSONFixed "ParameterLocation" JSON.withText toText

instance JSON.ToJSON ParameterLocation where
  toJSON = JSON.toJSON . toText

instance ToText ParameterLocation where
  toText = \case
    Cookie -> "cookie"
    Header -> "header"
    Path   -> "path"
    Query  -> "query"

type ParameterName = TaggedText "ParameterName"

data ParameterStyle = DeepObject | Form | Simple
  deriving stock (Eq, GHC.Bounded, GHC.Enum, Show)

instance JSON.FromJSON ParameterStyle where
  parseJSON = parseJSONFixed "ParameterStyle" JSON.withText toText

instance JSON.ToJSON ParameterStyle where
  toJSON = JSON.toJSON . toText

instance ToText ParameterStyle where
  toText = \case
    DeepObject -> "deepObject"
    Form       -> "form"
    Simple     -> "simple"

