module PGT.Prelude
  ( module Exports
  , showc
  )
where

import Data.Conversions    as Exports
import MPrelude            as Exports

showc :: forall b a . (Show a, Conversion b String) => a -> b
showc = convert . show
