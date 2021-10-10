{-# OPTIONS -Wno-redundant-constraints #-}

module OpenApi.Resolver (Resolver, resolve, resolveReferenceOr) where

import OpenApi.Components
import OpenApi.OpenApi
import OpenApi.Parameter
import OpenApi.Prelude
import OpenApi.Referencable
import OpenApi.Reference
import OpenApi.ReferenceOr
import OpenApi.RequestBody
import OpenApi.Response
import OpenApi.Schema

import qualified Data.List       as List
import qualified Data.Map.Strict as Map

class Referencable a => Resolver a where
  getMap :: OpenApi -> Maybe (Map (ComponentName a) (ReferenceOr a))

instance Resolver Schema where
  getMap = schemas <=< components

instance Resolver Response where
  getMap = responses <=< components

instance Resolver RequestBody where
  getMap = requestBodies <=< components

instance Resolver Parameter where
  getMap = parameters <=< components

resolveReferenceOr
  :: forall a . (Resolver a)
  => OpenApi
  -> Reference a
  -> Either (ResolverError a) (ReferenceOr a)
resolveReferenceOr components reference =
  maybe
    (Left EmptyComponentMap)
    (resolveReferenceOrWithMap [] reference)
    (getMap @a components)

data ResolverError a
  = EmptyComponentMap
  | MissingReference [Reference a] (Reference a)
  | ReferenceCycle [Reference a]
  deriving stock Show

resolve
  :: forall a . (Resolver a)
  => OpenApi
  -> Reference a
  -> Either (ResolverError a) a
resolve components reference =
  maybe
    (Left EmptyComponentMap)
    (resolveWithMap reference)
    (getMap @a components)

resolveWithMap
  :: forall a . (Resolver a)
  => Reference a
  -> Map (ComponentName a) (ReferenceOr a)
  -> Either (ResolverError a) a
resolveWithMap reference map = go [] reference
  where
    go
      :: [Reference a]
      -> Reference a
      -> Either (ResolverError a) a
    go stack current
      = case resolveReferenceOrWithMap stack current map of
          Right refOr -> unref stack refOr
          Left error -> Left error

    unref :: [Reference a] -> ReferenceOr a -> Either (ResolverError a) a
    unref stack = \case
      ReferenceTo next ->
        if next `List.elem` stack
          then Left $ ReferenceCycle (next:stack)
          else go (next:stack) next
      Literal value -> Right value

resolveReferenceOrWithMap
  :: forall a . (Resolver a)
  => [Reference a]
  -> Reference a
  -> Map (ComponentName a) (ReferenceOr a)
  -> Either (ResolverError a) (ReferenceOr a)
resolveReferenceOrWithMap stack reference
  = maybe (Left $ MissingReference stack reference) pure
  . Map.lookup (fromReference reference)
