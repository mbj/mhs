module OpenApi.Contact where

import OpenApi.JSON
import OpenApi.Prelude
import OpenApi.TaggedText

import qualified Data.Aeson as JSON

data Contact = Contact
  { email :: Maybe (TaggedText "ContactEmail")
  , name  :: Maybe (TaggedText "ContactName")
  , url   :: Maybe (TaggedText "ContactUrl")
  }
  deriving stock (Eq, Generic, Show)

instance JSON.FromJSON Contact where
  parseJSON = genericParseJSON
