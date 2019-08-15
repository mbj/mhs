module OpenApi.Description where

import OpenApi.Prelude

import qualified Data.Aeson as JSON

newtype Description a = Description Text
  deriving newtype (Eq, JSON.FromJSON, JSON.ToJSON, ToText)
  deriving stock   Show

class HasDescription a where
  getDescription :: a -> Maybe (Description a)
