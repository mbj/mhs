module OpenApi.Info where

import OpenApi.Contact
import OpenApi.JSON
import OpenApi.Prelude
import OpenApi.TaggedText

import qualified Data.Aeson as JSON

data Info = Info
  { contact        :: Maybe Contact
  , description    :: Maybe (TaggedText "InfoDescription")
  , termsOfService :: Maybe (TaggedText "InfoTermsOfService")
  , title          :: TaggedText "InfoTitle"
  , version        :: Maybe (TaggedText "InfoVersion")
  }
  deriving stock (Eq, Generic, Show)

instance JSON.FromJSON Info where
  parseJSON = genericParseJSON
