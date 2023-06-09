{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UndecidableInstances     #-}

module Data.Bounded.TypeLevel where

import Data.Bounded.Prelude
import Data.Word (Word16)
import GHC.TypeLits

import qualified Data.List as List

type family HasValidTypeRange (range :: (Nat, Nat)) (validity :: Bool) :: Constraint where
  HasValidTypeRange _ 'True            = ()
  HasValidTypeRange '(min, max) 'False =
    TypeError ('Text "Value "
               ':<>: 'ShowType min
               ':<>: 'Text " is out of range "
               ':<>: 'ShowType max
               )

type IsInRange (nat :: Nat) (min :: Nat) (max :: Nat)
    = ( HasValidTypeRange '(nat, max) (nat <=? max)
      , HasValidTypeRange '(min, nat) (min <=? nat)
      )

-- | Constructor for appending a type level string with a normal Type
data (a :: Symbol) ++: (b :: Type)

typeName :: forall (a :: Type). Typeable a => String
typeName
  = fromMaybe (error "GHC error invalid type name")
  . List.stripPrefix "Proxy * "
  . show
  $ typeOf (Proxy @a)

type Length :: Symbol -> Nat
type family Length (a :: Symbol) :: Nat where
  Length symbol = Length' (UnconsSymbol symbol)

type Length' :: Maybe (Char, Symbol) -> Nat
type family Length' symbol :: Nat where
  Length' 'Nothing                  = 0
  Length' ('Just '(char, reminder)) = 1 + Length' (UnconsSymbol reminder)

mkRange
  :: forall min max integral . (KnownNat min, KnownNat max, Conversion integral Natural)
  => (integral, integral)
mkRange = (convert $ fromType @min @Natural, convert $ fromType @max @Natural)

type family ToBoundedIntegral (a :: Type) :: Type where
  ToBoundedIntegral Natural = Word16
  ToBoundedIntegral Float   = TypeError ('Text "Float is not a bound integral")
  ToBoundedIntegral Double  = TypeError ('Text "Double is not a bound integral")
  ToBoundedIntegral a       = a
