module OpenApi.PathItem where

import OpenApi.JSON
import OpenApi.Operation
import OpenApi.Parameter
import OpenApi.Prelude
import OpenApi.ReferenceOr

import qualified Data.Aeson as JSON

data PathItem = PathItem
  { delete     :: Maybe Operation
  , get        :: Maybe Operation
  , head       :: Maybe Operation
  , options    :: Maybe Operation
  , patch      :: Maybe Operation
  , post       :: Maybe Operation
  , put        :: Maybe Operation
  , parameters :: Maybe [ReferenceOr Parameter]
  }
  deriving stock (Eq, Generic, Show)

instance JSON.FromJSON PathItem where
  parseJSON = genericParseJSON

