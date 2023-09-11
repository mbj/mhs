{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Bounded.Text
  ( BoundText
  , BoundText'
  , BoundTextError
  , convertTruncate
  )
where

import Data.Bounded.JSON
import Data.Bounded.Prelude
import Data.Bounded.TypeLevel
import GHC.TypeLits (type (<=?))

import qualified Data.Aeson                 as JSON
import qualified Data.Text                  as Text
import qualified Language.Haskell.TH.Syntax as TH
import qualified Text.Show                  as Show

type BoundText (label :: Symbol) = BoundText' label '(1, 128)

newtype BoundText' (a :: k) (range :: (Nat, Nat)) = BoundText Text
  deriving (Conversion Text) via Text
  deriving newtype (JSON.FromJSONKey, JSON.ToJSON, JSON.ToJSONKey)
  deriving stock (Eq, Ord, Show, TH.Lift, Typeable)

data BoundTextError = BoundTextError
  { actual :: Natural
  , label  :: String
  , min    :: Natural
  , max    :: Natural
  }

instance Show.Show BoundTextError where
  show BoundTextError{..}
    =  label
    <> " length should have been between "
    <> show min
    <> " and "
    <> show max
    <> " but was "
    <> show actual

instance Conversion Text BoundTextError where
  convert = convert . show

instance Exception BoundTextError

class BoundTextLabel label where
  labelName :: String

instance (KnownSymbol label) => BoundTextLabel (label :: Symbol) where
  labelName = fromType @label

instance (KnownSymbol a, Typeable b) => BoundTextLabel (a ++: b) where
  labelName = fromType @a <> " " <> typeName @b

instance
  ( KnownNat min, KnownNat max, BoundTextLabel label
  , HasValidTypeRange '(min, max) (min <=? max)
  )
  => JSON.FromJSON (BoundText' label '( min, max) ) where
  parseJSON value
    = BoundText
   <$> parseJSONTextBoundedLength (labelName @label) (mkRange @min @max) value

instance
  ( length ~ Length value
  , IsInRange length min max
  , KnownSymbol value
  )
  => FromType value (BoundText' label '( min, max)) where

  fromType = BoundText (fromType @value)

instance
  ( KnownNat min, KnownNat max
  , BoundTextLabel label
  , Conversion Text a
  , MonadError BoundTextError m
  )
  => Conversion (m (BoundText' label '(min, max))) a where
    convert (convert -> value) =
      if actual >= min && actual <= max
        then pure $ BoundText value
        else throwError $ BoundTextError{..}
      where
        actual :: Natural
        actual = convertImpure $ Text.length value

        label :: String
        label = labelName @label

        max :: Natural
        max = fromType @max

        min :: Natural
        min = fromType @min

instance
  ( HasValidTypeRange '(min2, min1) (min2 <=? min1)
  , HasValidTypeRange '(max1, max2) (max1 <=? max2)
  )
  => Conversion (BoundText' label2 '(min2, max2)) (BoundText' label1 '(min1, max1)) where
    convert (BoundText text) = BoundText text

convertTruncate
  :: forall min max label . (KnownNat min, KnownNat max)
  => Text
  -> Maybe (BoundText' label '(min, max))
convertTruncate value =
  if actual >= min
    then
      if actual <= max
        then pure $ BoundText value
        else pure $ BoundText $ Text.take (convertImpure max) value
      else empty
  where
    actual :: Natural
    actual = convertImpure $ Text.length value

    max :: Natural
    max = fromType @max

    min :: Natural
    min = fromType @min
