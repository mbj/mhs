module OpenApi.ResponseHeader where

import OpenApi.JSON
import OpenApi.Prelude
import OpenApi.TaggedText

import qualified Data.Aeson as JSON

data ResponseHeader = ResponseHeader
  { description :: Maybe (TaggedText "ResponseHeaderDescription")
  , name        :: TaggedText "ResponseHeaderName"
  }
  deriving anyclass JSON.ToJSON
  deriving stock    (Eq, Generic, Show)

instance JSON.FromJSON ResponseHeader where
  parseJSON = genericParseJSON
