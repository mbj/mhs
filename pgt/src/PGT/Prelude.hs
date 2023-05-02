module PGT.Prelude
  ( module Exports
  , showc
  )
where

import Control.Monad       as Exports (when)
import Data.Char           as Exports (Char)
import Data.Conversions    as Exports
import MPrelude            as Exports

showc :: forall b a . (Show a, Conversion b String) => a -> b
showc = convert . show
