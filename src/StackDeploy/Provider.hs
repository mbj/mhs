module StackDeploy.Provider (Get, Provider, get) where

import Control.Exception.Base (Exception)
import Control.Monad.Catch (MonadThrow, throwM)
import StackDeploy.Prelude

import qualified Data.Foldable as Foldable

type Provider a = [a]

type Get a b = forall m . MonadThrow m => Provider a -> b -> m a

newtype MissingProviderItem = MissingProviderItem Text
  deriving stock Show

instance Exception MissingProviderItem

get
  :: forall a b m . (Eq b, MonadThrow m, ToText b)
  => Text
  -> (a -> b)
  -> Provider a
  -> b
  -> m a
get subject accessor provider target
  = maybe failMissing pure $ Foldable.find ((==) target . accessor) provider
  where
    failMissing :: m a
    failMissing
      = throwM
      . MissingProviderItem
      $ "Unknown " <> subject <> ": " <> toText target
