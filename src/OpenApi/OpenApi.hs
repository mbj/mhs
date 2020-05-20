module OpenApi.OpenApi (OpenApi(..)) where

import OpenApi.Components
import OpenApi.Info
import OpenApi.JSON
import OpenApi.Paths
import OpenApi.Prelude
import OpenApi.SecurityRequirement
import OpenApi.Server
import OpenApi.Tag
import OpenApi.TaggedText

import qualified Data.Aeson as JSON

data OpenApi = OpenApi
  { components :: Maybe Components
  , info       :: Info
  , openapi    :: TaggedText "OpenApiVersion"
  , paths      :: Paths
  , security   :: Maybe [SecurityRequirement]
  , servers    :: Maybe [Server]
  , tags       :: Maybe [Tag]
  , xTagGroups :: Maybe JSON.Value
  }
  deriving stock (Eq, Generic, Show)

jsonRenames :: Map String String
jsonRenames =
  [ ("xTagGroups", "x-tag-groups")
  ]

instance JSON.FromJSON OpenApi where
  parseJSON = parseRenamed jsonRenames
