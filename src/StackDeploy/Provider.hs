module StackDeploy.Provider (Provider, get) where

import StackDeploy.Prelude

import qualified Data.Foldable as Foldable

type Provider a = forall m . (MonadIO m) => m [a]

get
  :: forall a b m . (Eq b, MonadIO m, ToText b)
  => Text
  -> (a -> b)
  -> m [a]
  -> b
  -> m a
get subject accessor provider target
  =   maybe failMissing pure
  .   Foldable.find ((==) target . accessor)
  =<< provider
  where
    failMissing :: m a
    failMissing
      = liftIO
      . fail
      . convertText
      $ "Unknown " <> subject <> ": " <> toText target
