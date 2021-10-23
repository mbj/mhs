{-# LANGUAGE CPP #-}

-- | Conversions from type-level values
--
module Data.Conversions.FromType where

import           Data.Conversions               ( convert )
import           Data.Function                  ( ($) )
import           Data.Kind                      ( Type )
import           Data.Proxy                     ( Proxy(..) )
import           Data.String                    ( String )
import           Data.Text                      ( Text )
import           GHC.TypeLits            hiding ( natVal )
import           GHC.TypeNats                   ( natVal )

#if !MIN_VERSION_GLASGOW_HASKELL(9,2,0,0)
import           Numeric.Natural                ( Natural )
#endif

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
