module Database.Migration.Prelude
  ( module Exports
  , eitherFail
  )
where

import Data.Conversions as Exports
import MPrelude         as Exports

eitherFail :: (MonadFail m, Show e) => Either e a -> m a
eitherFail = either (fail . show) pure
