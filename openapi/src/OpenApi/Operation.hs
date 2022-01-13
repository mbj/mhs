module OpenApi.Operation where

import OpenApi.JSON
import OpenApi.Parameter
import OpenApi.Prelude
import OpenApi.ReferenceOr
import OpenApi.RequestBody
import OpenApi.Responses
import OpenApi.SecurityRequirement
import OpenApi.Tag
import OpenApi.TaggedText

import qualified Data.Aeson as JSON

data Operation = Operation
  { deprecated  :: Maybe Bool
  , description :: Maybe (TaggedText "OperationDescription")
  , operationId :: TaggedText "OperationId"
  , parameters  :: Maybe [ReferenceOr Parameter]
  , requestBody :: Maybe (ReferenceOr RequestBody)
  , responses   :: Responses
  , security    :: Maybe [SecurityRequirement]
  , summary     :: Maybe (TaggedText "OperationSummary")
  , tags        :: Maybe [TagName]
  }
  deriving anyclass JSON.ToJSON
  deriving stock    (Eq, Generic, Show)

instance JSON.FromJSON Operation where
  parseJSON = genericParseJSON
