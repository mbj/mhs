module OpenApi.Info (Info(..)) where

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
  , xLogo          :: Maybe JSON.Value
  }
  deriving stock (Eq, Generic, Show)

jsonRenames :: Map String String
jsonRenames =
  [ ("xLogo", "x-logo")
  ]

instance JSON.FromJSON Info where
  parseJSON = parseRenamed jsonRenames
