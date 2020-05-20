module OpenApi.SecurityRequirement where

import OpenApi.JSON
import OpenApi.Prelude
import OpenApi.TaggedText

import qualified Data.Aeson as JSON

newtype SecurityRequirement = SecurityRequirement
  { apiKey :: Maybe [TaggedText "SecurityRequirementScope"]
  }
  deriving stock (Eq, Generic, Show)

instance JSON.FromJSON SecurityRequirement where
  parseJSON = genericParseJSON
