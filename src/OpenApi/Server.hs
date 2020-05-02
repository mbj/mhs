module OpenApi.Server where

import OpenApi.JSON
import OpenApi.Prelude
import OpenApi.TaggedText

import qualified Data.Aeson as JSON

data Server = Server
  { url            :: TaggedText "ServerUrl"
  , description    :: Maybe (TaggedText "ServerDescription")
  }
  deriving stock (Eq, Generic, Show)

instance JSON.FromJSON Server where
  parseJSON = genericParseJSON
