module OpenApi.Schema where

import Data.Scientific (Scientific)
import OpenApi.JSON
import OpenApi.Prelude hiding (Enum)
import OpenApi.Referencable
import OpenApi.Reference
import OpenApi.ReferenceOr
import OpenApi.TaggedText

import qualified Data.Aeson       as JSON
import qualified Data.Aeson.Types as JSON
import qualified GHC.Enum         as GHC

newtype TaggedNatural (label :: Symbol) = TaggedNatural Natural
  deriving newtype (JSON.FromJSON, JSON.ToJSON)
  deriving stock   (Eq, Show)

type PropertyName = TaggedText "PropertyName"

data AdditionalProperties
  = AdditionalPropertiesBool Bool
  | AdditionalPropertiesSchema (ReferenceOr Schema)
  deriving stock (Eq, Show)

instance JSON.FromJSON AdditionalProperties where
  parseJSON = \case
    value@(JSON.Object _) -> AdditionalPropertiesSchema <$> JSON.parseJSON value
    (JSON.Bool bool)      -> pure $ AdditionalPropertiesBool bool
    value                 -> JSON.typeMismatch "Object or Bool" value

instance JSON.ToJSON AdditionalProperties where
  toJSON = \case
    AdditionalPropertiesBool bool     -> JSON.toJSON bool
    AdditionalPropertiesSchema schema -> JSON.toJSON schema

type Properties = Map PropertyName (ReferenceOr Schema)

data Schema = Schema
  { additionalProperties :: Maybe AdditionalProperties
  , allOf                :: Maybe [ReferenceOr Schema]
  , anyOf                :: Maybe [ReferenceOr Schema]
  , default'             :: Maybe JSON.Value
  , deprecated           :: Maybe Bool
  , description          :: Maybe (TaggedText "SchemaDescription")
  , discriminator        :: Maybe Discriminator
  , enum                 :: Maybe Enum
  , example              :: Maybe JSON.Value
  , exclusiveMaximum     :: Maybe Bool
  , exclusiveMinimum     :: Maybe Bool
  , format               :: Maybe Format
  , items                :: Maybe (ReferenceOr Schema)
  , maxItems             :: Maybe (TaggedNatural "MaxItems")
  , maxLength            :: Maybe (TaggedNatural "MaxLength")
  , maximum              :: Maybe Scientific
  , minItems             :: Maybe (TaggedNatural "MinLength")
  , minLength            :: Maybe (TaggedNatural "MinLength")
  , minimum              :: Maybe Scientific
  , multipleOf           :: Maybe Scientific
  , not                  :: Maybe (ReferenceOr Schema)
  , nullable             :: Maybe Bool
  , oneOf                :: Maybe [ReferenceOr Schema]
  , pattern'             :: Maybe (TaggedText "SchemaPattern")
  , properties           :: Maybe Properties
  , required             :: Maybe [PropertyName]
  , title                :: Maybe (TaggedText "SchemaTitle")
  , type'                :: Maybe Type
  , uniqueItems          :: Maybe Bool
  , xExamples            :: Maybe JSON.Object
  , xExpandableFields    :: Maybe [PropertyName]
  , xResourceId          :: Maybe (TaggedText "SchemaResourceId")
  }
  deriving stock (Eq, Generic, Show)

instance Referencable Schema where
  targetName    = "Schema"
  referencePath = ["components", "schemas"]

schemaObjectRenames :: Map String String
schemaObjectRenames =
  [ ("default'",          "default")
  , ("pattern'",          "pattern")
  , ("type'",             "type")
  , ("xExamples",         "x-examples")
  , ("xExpandableFields", "x-expandableFields")
  , ("xResourceId",       "x-resourceId")
  ]

instance JSON.FromJSON Schema where
  parseJSON = parseRenamed schemaObjectRenames

instance JSON.ToJSON Schema where
  toJSON = generateRenamed schemaObjectRenames

data Type = Array | Boolean | Integer | Number | Object | String
  deriving stock (Eq, GHC.Bounded, GHC.Enum, Show)

type DiscriminatorKey = TaggedText "DiscriminatorKey"

data Discriminator = Discriminator
  { mapping      :: Maybe (Map DiscriminatorKey (Reference Schema))
  , propertyName :: PropertyName
  }
  deriving anyclass (JSON.FromJSON, JSON.ToJSON)
  deriving stock (Eq, Generic, Show)

instance ToText Type where
  toText = \case
    Array   -> "array"
    Boolean -> "boolean"
    Integer -> "integer"
    Number  -> "number"
    Object  -> "object"
    String  -> "string"

instance JSON.FromJSON Type where
  parseJSON = parseJSONFixed "Type" JSON.withText toText

instance JSON.ToJSON Type where
  toJSON = JSON.toJSON . toText

newtype Enum = Enum [JSON.Value]
  deriving newtype (JSON.FromJSON, JSON.ToJSON)
  deriving stock   (Eq, Show)

data Format
  = CustomFormat Text  -- Non standard formats, explicitly allowed by OAS.
  | UnixTime
  deriving stock (Eq, Show)

instance ToText Format where
  toText = \case
    CustomFormat format -> format
    UnixTime            -> "unix-time"

instance JSON.FromJSON Format where
  parseJSON = JSON.withText "format" $ \case
    "unix-time" -> pure UnixTime
    format      -> pure $ CustomFormat format

instance JSON.ToJSON Format where
  toJSON = JSON.toJSON . toText
