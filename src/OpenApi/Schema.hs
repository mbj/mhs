module OpenApi.Schema where

import Data.Aeson.Types (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Map.Strict (Map)
import Data.String (String)
import Data.Tuple (snd, uncurry)
import GHC.Generics
import Numeric.Natural
import OpenApi.JSON
import OpenApi.Prelude
import Prelude (undefined)

import qualified Data.Aeson          as JSON
import qualified Data.Aeson.Types    as JSON
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List           as List
import qualified Data.Map.Strict     as Map
import qualified GHC.Enum            as GHC

data AdditionalProperties
  = AdditionalPropertiesBool Bool
  | AdditionalPropertiesSchema Schema
  deriving stock Show

instance FromJSON AdditionalProperties where
  parseJSON = \case
    value@(JSON.Object _) -> AdditionalPropertiesSchema <$> JSON.parseJSON value
    (JSON.Bool bool)      -> pure $ AdditionalPropertiesBool bool
    value                 -> JSON.typeMismatch "Object or Bool" value

instance ToJSON AdditionalProperties where
  toJSON = \case
    AdditionalPropertiesBool bool     -> JSON.toJSON bool
    AdditionalPropertiesSchema schema -> JSON.toJSON schema

newtype MaxLength = MaxLength Natural
  deriving newtype (FromJSON, ToJSON)
  deriving stock   Show

newtype MaxProperties = MaxProperties Natural
  deriving newtype (FromJSON, ToJSON)
  deriving stock   Show

newtype MinLength = MinLength Natural
  deriving newtype (FromJSON, ToJSON)
  deriving stock   Show

newtype MinProperties = MinProperties Natural
  deriving newtype (FromJSON, ToJSON)
  deriving stock   Show

data Properties = Properties (Map PropertyName Schema) | EmptyProperties
  deriving stock Show

instance FromJSON Properties where
  parseJSON = JSON.withObject "properties" $ \map ->
    if HashMap.null map
      then pure EmptyProperties
      else Properties <$> JSON.parseJSON (JSON.Object map)

instance ToJSON Properties where
  toJSON = \case
    EmptyProperties -> JSON.object empty
    Properties map  -> JSON.toJSON map

newtype PropertyName = PropertyName Text
  deriving newtype (Eq, FromJSON, FromJSONKey, Ord, ToJSON, ToJSONKey, ToText)
  deriving stock   Show

newtype MultipleOf = MultipleOf Natural
  deriving newtype (FromJSON, ToJSON)
  deriving stock   Show

newtype ResourceId = ResourceId Text
  deriving newtype (FromJSON, ToJSON)
  deriving stock   Show

newtype Name = Name Text
  deriving newtype (Eq, FromJSONKey, Ord, ToJSON, ToJSONKey, ToText)
  deriving stock   Show

newtype Pattern = Pattern Text
  deriving newtype (FromJSON, ToJSON, ToText)
  deriving stock   Show

data Schema = Content SchemaObject | Reference Name
  deriving stock Show

instance FromJSON Schema where
  parseJSON = parseRefSum Name Content Reference "#/components/schemas/" "Schema"

instance ToJSON Schema where
  toJSON = \case
    Content schemaObject ->
      JSON.toJSON schemaObject
    Reference name ->
      JSON.object [("$ref", JSON.toJSON $ "#/components/schemas/" <> toText name)]

data SchemaObject = SchemaObject
  { additionalProperties :: Maybe AdditionalProperties
  , allOf                :: Maybe [Schema]
  , anyOf                :: Maybe [Schema]
  , default'             :: Maybe JSON.Value
  , deprecated           :: Maybe Bool
  , description          :: Maybe Description
  , enum                 :: Maybe Enum
  , exclusiveMaximum     :: Maybe Bool
  , exclusiveMinimum     :: Maybe Bool
  , format               :: Maybe Format
  , items                :: Maybe Schema
  , maxLength            :: Maybe MaxLength
  , minLength            :: Maybe MinLength
  , not                  :: Maybe Schema
  , nullable             :: Maybe Bool
  , oneOf                :: Maybe [Schema]
  , pattern'             :: Maybe Pattern
  , properties           :: Maybe Properties
  , required             :: Maybe [PropertyName]
  , title                :: Maybe Title
  , type'                :: Maybe Type
  , uniqueItems          :: Maybe Bool
  , xExpandableFields    :: Maybe [PropertyName]
  , xResourceId          :: Maybe ResourceId
  }
  deriving stock (Generic, Show)

schemaObjectRenames :: Map String String
schemaObjectRenames = Map.fromList
  [ ("default'",          "default")
  , ("pattern'",          "pattern")
  , ("type'",             "type")
  , ("xExpandableFields", "x-expandableFields")
  , ("xResourceId",       "x-resourceId")
  ]

instance FromJSON SchemaObject where
  parseJSON = parseRenamed schemaObjectRenames

instance ToJSON SchemaObject where
  toJSON = generateRenamed schemaObjectRenames

toJSONSchema :: SchemaObject -> JSON.Object
toJSONSchema schemaObject = case JSON.toJSON schemaObject of
  JSON.Object object -> collapseObject object
  -- TODO refactor so the impure exception goes away.
  -- It requires that we define this function natively via re-using the
  -- generic mechanisms provided by aeson.
  _                  -> undefined
  where
    collapse :: JSON.Value -> JSON.Value
    collapse = \case
      (JSON.Array values)  -> JSON.Array $ collapse <$> values
      (JSON.Object object) -> JSON.Object $ collapseObject object
      value                -> value

    collapseObject :: JSON.Object -> JSON.Object
    collapseObject = HashMap.fromList . collapsePairs . HashMap.toList

    collapsePairs :: [JSON.Pair] -> [JSON.Pair]
    collapsePairs
      = (uncurry collapsePair <$>)
      . List.filter ((/= JSON.Null) . snd)

    collapsePair :: Text -> JSON.Value -> JSON.Pair
    collapsePair text value = (text, collapse value)

newtype Description = Description Text
  deriving newtype (FromJSON, ToJSON, ToText)
  deriving stock   Show

newtype Title = Title Text
  deriving newtype (FromJSON, ToJSON)
  deriving stock   Show

data Type = Array | Boolean | Integer | Number | Object | String
  deriving stock (GHC.Bounded, GHC.Enum, Show)

instance ToText Type where
  toText = \case
    Array   -> "array"
    Boolean -> "boolean"
    Integer -> "integer"
    Number  -> "number"
    Object  -> "object"
    String  -> "string"

instance FromJSON Type where
  parseJSON = parseJSONFixed "Type" JSON.withText toText

instance ToJSON Type where
  toJSON = JSON.toJSON . toText

newtype Enum = Enum [JSON.Value]
  deriving newtype (FromJSON, ToJSON)
  deriving stock Show

data Format = UnixTime
  deriving stock (GHC.Bounded, GHC.Enum, Show)

instance ToText Format where
  toText = \case
    UnixTime -> "unix-time"

instance FromJSON Format where
  parseJSON = parseJSONFixed "Format" JSON.withText toText

instance ToJSON Format where
  toJSON = JSON.toJSON . toText
