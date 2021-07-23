module Data.Bounded.Prelude
  ( module Exports
  , showc
  ) where

import Control.Applicative       as Exports (empty)
import Control.Exception         as Exports (Exception, try)
import Control.Monad             as Exports ((<=<), unless)
import Control.Monad.Error.Class as Exports (MonadError, throwError)
import Data.Conversions          as Exports
import Data.Conversions.FromType as Exports
import Data.Kind                 as Exports
import Data.Maybe                as Exports (fromMaybe)
import Data.Proxy                as Exports
import Data.Text                 as Exports (Text)
import Data.Typeable             as Exports (Typeable, typeOf)
import GHC.Generics              as Exports (Generic)
import GHC.Natural               as Exports (Natural)
import GHC.TypeLits              as Exports (KnownNat, KnownSymbol, Nat, Symbol)
import Prelude                   as Exports hiding (length, max, min)

showc :: forall b a . (Show a, Conversion b String) => a -> b
showc = convert . show
