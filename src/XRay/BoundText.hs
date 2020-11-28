{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE UndecidableInstances #-}

module XRay.BoundText
  ( BoundText
  , BoundTextError
  )
where

import Control.Exception (Exception)
import Control.Monad.Except (MonadError, throwError)
import Data.Hashable (Hashable)
import Data.Kind (Constraint, Type)
import Data.Symbol.Ascii (ToList)
import Data.Typeable (Proxy(..), Typeable, typeOf)
import GHC.TypeLits
import Prelude (error)
import XRay.Prelude

import qualified Data.Aeson as JSON
import qualified Data.List  as List
import qualified Data.Text  as Text
import qualified Text.Show  as Show

newtype BoundText (a :: k) (range :: (Nat, Nat)) = BoundText Text
  deriving (Conversion Text) via Text
  deriving newtype (Hashable, JSON.ToJSON, JSON.ToJSONKey)
  deriving stock (Eq, Ord, Show, Typeable)

class BoundTextLabel label where
  labelName :: String

instance (KnownSymbol label) => BoundTextLabel (label :: Symbol) where
  labelName = fromType @label

instance (KnownSymbol a, Typeable b) => BoundTextLabel (a ++: b) where
  labelName = fromType @a <> " " <> typeName @b

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

instance Exception BoundTextError

-- | Constructor for appending a type level string with a normal Type
data (a :: Symbol) ++: (b :: Type)

type family (++) (a :: Symbol) (b :: k) :: Symbol where
  a ++ (b :: Symbol) = a `AppendSymbol` b

typeName :: forall (a :: Type). Typeable a => String
typeName
  = fromMaybe (error "GHC error invalid type name")
  . List.stripPrefix "Proxy * "
  . show
  $ typeOf (Proxy @a)

instance
  ( KnownNat min, KnownNat max
  , BoundTextLabel label
  , Conversion Text a
  , MonadError BoundTextError m
  )
  => Conversion (m (BoundText label '(min, max))) a where
    convert value =
      if actual >= min && actual <= max
        then pure $ BoundText text
        else throwError $ BoundTextError{..}
      where
        actual :: Natural
        actual = convertUnsafe $ Text.length text

        label :: String
        label = labelName @label

        max :: Natural
        max = fromType @max

        min :: Natural
        min = fromType @min

        text = convert value

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

type family Length (a :: Symbol) :: Nat where
  Length symbol = Length' (ToList symbol)

type family Length' (a :: [Symbol]) :: Nat where
  Length' '[] = 0
  Length' (x ': xs) = 1 + Length' xs

instance
  ( length ~ Length value
  , IsInRange length min max
  , KnownSymbol value
  )
  => FromType value (BoundText label '( min, max)) where

  fromType = BoundText (fromType @value)
