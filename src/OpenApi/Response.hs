module OpenApi.Response where

import OpenApi.JSON
import OpenApi.MediaType
import OpenApi.MediaTypeQuery
import OpenApi.Prelude
import OpenApi.Reference
import OpenApi.ResponseHeader
import OpenApi.TaggedText

import qualified Data.Aeson as JSON

data Response = Response
  { content     :: Map MediaTypeQuery MediaType
  , description :: TaggedText "ResponseDescription"
  , headers     :: Maybe (Map Text ResponseHeader)
  }
  deriving anyclass JSON.ToJSON
  deriving stock    (Eq, Generic, Show)

instance Referencable Response where
  targetName    = "Response"
  referencePath = ["components", "responses"]

instance JSON.FromJSON Response where
  parseJSON = genericParseJSON
