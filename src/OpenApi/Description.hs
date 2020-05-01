module OpenApi.Description where

import OpenApi.Prelude
import OpenApi.TaggedText

type Description = TaggedText "Description"

class HasDescription a where
  getDescription :: a -> Maybe (Description a)
