module OpenApi.RequestBody where

import OpenApi.JSON
import OpenApi.MediaType
import OpenApi.MediaTypeQuery
import OpenApi.Prelude
import OpenApi.Reference
import OpenApi.TaggedText

import qualified Data.Aeson as JSON

data RequestBody = RequestBody
  { content     :: Map MediaTypeQuery MediaType
  , description :: Maybe (TaggedText "RequestBody")
  , required    :: Maybe Bool
  }
  deriving anyclass JSON.ToJSON
  deriving stock    (Eq, Generic, Show)

instance Referencable RequestBody where
  targetName    = "Parameter"
  referencePath = ["components", "requestBodies"]

instance JSON.FromJSON RequestBody where
  parseJSON = genericParseJSON

