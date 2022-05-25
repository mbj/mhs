module OpenApi.ResponseHeader where

import OpenApi.JSON
import OpenApi.Prelude
import OpenApi.ReferenceOr
import OpenApi.Schema
import OpenApi.TaggedText

import qualified Data.Aeson as JSON

data ResponseHeader = ResponseHeader
  { description :: Maybe (TaggedText "ResponseHeaderDescription")
  , schema      :: Maybe (ReferenceOr Schema)
  }
  deriving anyclass JSON.ToJSON
  deriving stock    (Eq, Generic, Show)

instance JSON.FromJSON ResponseHeader where
  parseJSON = genericParseJSON
