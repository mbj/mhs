module OpenApi.OpenApi where

import OpenApi.Components
import OpenApi.Info
import OpenApi.JSON
import OpenApi.Paths
import OpenApi.Prelude
import OpenApi.Server
import OpenApi.Tag
import OpenApi.TaggedText

import qualified Data.Aeson as JSON

data OpenApi = OpenApi
  { components :: Maybe Components
  , info       :: Info
  , openapi    :: TaggedText "OpenApiVersion"
  , paths      :: Paths
  , servers    :: Maybe [Server]
  , tags       :: Maybe [Tag]
  }
  deriving stock (Eq, Generic, Show)

instance JSON.FromJSON OpenApi where
  parseJSON = genericParseJSON
