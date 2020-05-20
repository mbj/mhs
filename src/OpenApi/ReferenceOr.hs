module OpenApi.ReferenceOr where

import OpenApi.Prelude
import OpenApi.Referencable
import OpenApi.Reference

import qualified Data.Aeson          as JSON
import qualified Data.Aeson.Types    as JSON
import qualified Data.HashMap.Strict as HashMap

data ReferenceOr a = ReferenceTo (Reference a) | Literal a
  deriving stock (Eq, Show)

instance (Referencable a, JSON.FromJSON a) => JSON.FromJSON (ReferenceOr a) where
  parseJSON input = JSON.withObject (targetName @a) parseObject input
    where
      parseObject :: JSON.Object -> JSON.Parser (ReferenceOr a)
      parseObject object
        = maybe (Literal <$> JSON.parseJSON input) (parseReference object)
        $ HashMap.lookup "$ref" object

      parseReference :: JSON.Object -> JSON.Value -> JSON.Parser (ReferenceOr a)
      parseReference object value =
        if HashMap.size object == 1
          then ReferenceTo <$> JSON.parseJSON value
          else fail "$ref key with siblings"

instance (Referencable a, JSON.ToJSON a) => JSON.ToJSON (ReferenceOr a) where
  toJSON = \case
    Literal value     -> JSON.toJSON value
    ReferenceTo value -> JSON.toJSON value
