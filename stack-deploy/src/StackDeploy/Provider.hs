{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}

module StackDeploy.Provider (Get, HasItemName(..), Provider, get) where

import Control.Exception.Base (Exception)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.MonoTraversable (Element, MonoFunctor)
import StackDeploy.Prelude

import qualified Data.List       as List
import qualified Data.Map.Strict as Map

type instance Element (Provider a) = a

newtype Provider a = Provider (Map (ItemName a) a)
  deriving newtype MonoFunctor

deriving newtype instance (Eq a, Eq (ItemName a))     => Eq   (Provider a)
deriving newtype instance (Show a, Show (ItemName a)) => Show (Provider a)

class HasItemName a where
  type ItemName a :: Type
  name :: a -> ItemName a

instance (HasItemName a, Ord (ItemName a)) => IsList (Provider a) where
  type Item (Provider a) = a

  fromList items = Provider $ Map.fromList (mkPair <$> items)
    where
      mkPair :: a -> (ItemName a, a)
      mkPair item = (name item, item)

  toList (Provider map) = List.sortOn name $ Map.elems map

type Get a b = forall m . MonadThrow m => Provider a -> b -> m a

newtype MissingProviderItem = MissingProviderItem Text
  deriving stock Show

instance Exception MissingProviderItem

get
  :: forall a m . (MonadThrow m, Ord (ItemName a), ToText (ItemName a))
  => Text
  -> Provider a
  -> ItemName a
  -> m a
get subject (Provider map) targetName
  = maybe failMissing pure $ Map.lookup targetName map
  where
    failMissing :: m a
    failMissing
      = throwM
      . MissingProviderItem
      $ "Unknown " <> subject <> ": " <> toText targetName

