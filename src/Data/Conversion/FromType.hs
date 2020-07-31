-- | Conversions from type-level values
--
module Data.Conversion.FromType where

import Data.Conversion
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import GHC.TypeLits hiding (natVal)
import GHC.TypeNats (natVal)
import Numeric.Natural (Natural)
import Prelude

class FromType a (b :: Type) where
  fromType :: b

-- These instances are not polymorphic in the last parameter so to
-- avoid over-lapping instances
--
instance (KnownNat nat) => FromType nat Natural where
  fromType = natVal (Proxy @nat)

instance (KnownSymbol symbol) => FromType symbol String where
  fromType = symbolVal (Proxy @symbol)

instance (KnownSymbol symbol) => FromType symbol Text where
  fromType = convert @Text @String $ fromType @symbol
