{-# LANGUAGE UndecidableInstances #-}

module Data.Bounded.Integral where

import Data.Bounded.JSON
import Data.Bounded.Prelude
import Data.Bounded.TypeLevel
import GHC.TypeLits (type (<=?))


import qualified Data.Aeson as JSON
import qualified Data.Csv   as CSV

newtype BoundNumber' (integral :: Type) (label :: Symbol) (range :: (Nat, Nat))
  = BoundNumber { unBoundNumber :: integral }
  deriving newtype (JSON.ToJSON)
  deriving stock (Eq, Ord, Show, Typeable)

type BoundNumber = BoundNumber' Natural

instance Conversion Natural integral
  => Conversion Natural (BoundNumber' integral label range) where
  convert = convert . unBoundNumber

instance Show integral => Conversion Text (BoundNumber' integral label range) where
  convert = showc . unBoundNumber

instance
  ( bound ~ BoundNumber' integral label '(min, max)
  , Bounded bound
  , Conversion integral Natural
  , Conversion Natural integral
  , boundNumberError ~ UserBoundError Natural bound
  , MonadError boundNumberError m
  )
  => Conversion (m (BoundNumber' integral label '(min, max))) Natural where

  convert nat
    | nat <= maxBound' && nat >= minBound'
        = pure . BoundNumber $ convert nat
    | otherwise
        = throwError $ UserBoundError nat minBound maxBound
    where
      maxBound' :: Natural
      maxBound' = convert @Natural $ maxBound @bound

      minBound' :: Natural
      minBound' = convert @Natural $ minBound @bound

instance (KnownNat min, KnownNat max, Conversion integral Natural )
  => Bounded (BoundNumber' integral label '(min, max)) where
  minBound = BoundNumber . convert @integral $ fromType @min @Natural

  maxBound = BoundNumber . convert @integral $ fromType @max @Natural

instance
  ( KnownNat min, KnownNat max, KnownSymbol label
  , HasValidTypeRange '(min, max) (min <=? max)
  , Conversion integral (ToBoundedIntegral integral)
  , Conversion integral Natural
  , Bounded (ToBoundedIntegral integral)
  , Integral (ToBoundedIntegral integral)
  , Integral integral
  , Show integral
  )
  => JSON.FromJSON (BoundNumber' integral label '(min, max)) where
  parseJSON value
    = BoundNumber
   <$> parseJSONIntegralBounded (fromType @label) (mkRange @min @max @integral) value

instance
  ( KnownNat min, KnownNat max
  , HasValidTypeRange '(min, max) (min <=? max)
  )
  => CSV.FromField (BoundNumber label '(min, max)) where
  parseField = convertFail @(BoundNumber label '(min, max)) <=< CSV.parseField @Natural

instance
  ( KnownNat nat
  , IsInRange nat min max
  , Conversion integral Natural
  )
  => FromType (nat :: Nat) (BoundNumber' integral label '(min, max)) where

  fromType = BoundNumber . convert @integral $ fromType @nat @Natural
