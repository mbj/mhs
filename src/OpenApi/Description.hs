module OpenApi.Description where

import OpenApi.Prelude

import qualified Data.Aeson as JSON

newtype Description a = Description Text
  deriving newtype (JSON.FromJSON, JSON.ToJSON, ToText)
  deriving stock   Show
