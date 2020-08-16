module StackDeploy.Provider (Get, HasName(..), Name, Provider, get, mkName) where

import Control.Exception.Base (Exception)
import Data.HashMap.Strict (HashMap)
import Data.Hashable (Hashable)
import StackDeploy.Prelude

import qualified Data.HashMap.Strict as HashMap
import qualified Data.List           as List

newtype Name a = Name Text
  deriving newtype (Hashable, ToText)
  deriving stock   (Eq, Ord, Show)

newtype Provider a = Provider (HashMap (Name a) a)

class HasName a where
  name :: a -> Name a

instance HasName a => IsList (Provider a) where
  type Item (Provider a) = a

  fromList items = Provider $ HashMap.fromList (mkPair <$> items)
    where
      mkPair :: a -> (Name a, a)
      mkPair item = (name item, item)

  toList (Provider map) = List.sortOn name $ HashMap.elems map

type Get a b = forall m . MonadThrow m => Provider a -> b -> m a

newtype MissingProviderItem = MissingProviderItem Text
  deriving stock Show

instance Exception MissingProviderItem

get
  :: forall a m . (MonadThrow m)
  => Text
  -> Provider a
  -> Name a
  -> m a
get subject (Provider map) targetName
  = maybe failMissing pure $ HashMap.lookup targetName map
  where
    failMissing :: m a
    failMissing
      = throwM
      . MissingProviderItem
      $ "Unknown " <> subject <> ": " <> toText targetName

mkName :: Text -> Name a
mkName = Name
