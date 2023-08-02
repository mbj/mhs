module PGT.Prelude
  ( module Exports
  , showc
  )
where

import Control.Applicative  as Exports ((*>), (<*), optional)
import Control.Monad        as Exports (when)
import Control.Monad.Reader as Exports (ask, asks)
import Data.Char            as Exports (Char)
import Data.Conversions     as Exports
import Data.Vector          as Exports (Vector)
import MIO.Core             as Exports
import MPrelude             as Exports
import UnliftIO.Exception   as Exports (throwIO, throwString)

showc :: forall b a . (Show a, Conversion b String) => a -> b
showc = convert . show
