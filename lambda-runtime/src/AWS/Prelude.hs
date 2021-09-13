module AWS.Prelude
  ( module Exports
  , maybeToEither
  , safeHead
  , showc
  ) where

import Control.Exception      as Exports (Exception)
import Control.Monad          as Exports (forever, unless, when)
import Control.Monad.Catch    as Exports (MonadCatch, catch, throwM, try)
import Data.Bifunctor         as Exports
import Data.Conversions       as Exports
import Data.Text.Encoding     as Exports (decodeUtf8, encodeUtf8)
import MPrelude               as Exports

showc :: forall b a . (Show a, Conversion b String) => a -> b
showc = convert . show

safeHead :: [a] -> Maybe a
safeHead = \case
  (x:_) -> pure x
  _     -> empty

maybeToEither :: b -> Maybe a -> Either b a
maybeToEither error = maybe (Left error) Right
