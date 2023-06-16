module Data.Conversions.Hedgehog
  ( BiConvert(..)
  , Generators(..)
  , mkConvertEither
  , run
  )
where

import Control.Arrow (left)
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Data.String (fromString)
import Data.Typeable (Proxy(..), Typeable, typeOf)
import MPrelude

import qualified Data.List           as List
import qualified GHC.Err             as Err
import qualified Hedgehog
import qualified Test.Tasty          as Tasty
import qualified Test.Tasty.Hedgehog as Tasty

type PropertyIO = Hedgehog.PropertyT IO ()

data Generators a = Generators
  { success :: NonEmpty (Hedgehog.Gen a)
  , failure :: [Hedgehog.Gen a]
  }

data Expectation = Failure | Success
  deriving stock (Eq, Show)

newtype ConversionError = ConversionError String
  deriving stock (Show, Eq)

data BiConvert target source = BiConvert
  { fromTarget :: target -> Either ConversionError source
  , toTarget   :: source -> Either ConversionError target
  }

run
  :: forall target source
  .  (Show source, Show target, Typeable source, Typeable target)
  => BiConvert target source
  -> Generators source
  -> Tasty.TestTree
run BiConvert{..} Generators{failure = failure', ..}
  = Tasty.fromGroup
  $ Hedgehog.Group groupName process
  where
    groupName :: Hedgehog.GroupName
    groupName
      =  fromString
      $  typeName @source
      <> " to "
      <> typeName @target
      <> " conversion"

    process :: [(Hedgehog.PropertyName, Hedgehog.Property)]
    process
      = [ mkGroupItem Success $ runGeneratorList (roundTrip Success) success
        , mkGroupItem Failure $ runGeneratorList (roundTrip Failure) failure'
        ]
        where
          mkGroupItem :: Expectation -> Hedgehog.Property -> (Hedgehog.PropertyName, Hedgehog.Property)
          mkGroupItem expectation = (fromString $ "assert " <> show expectation,)

          runGeneratorList
            :: Foldable f
            => (source -> PropertyIO)
            -> f (Hedgehog.Gen source)
            -> Hedgehog.Property
          runGeneratorList assertion = Hedgehog.property . traverse_ (assertion <=< Hedgehog.forAll)

          roundTrip
            :: Expectation
            -> source
            -> PropertyIO
          roundTrip expectation source = case expectation of
            Success -> onSuccess $ toTarget source
            Failure -> onFailure $ toTarget source
            where
              onSuccess :: Either ConversionError target -> PropertyIO
              onSuccess = either mkError (either mkError (const (pure ())) . fromTarget)

              onFailure :: Either ConversionError target -> PropertyIO
              onFailure = either (const (pure ())) mkError

              mkError :: Show a => a -> PropertyIO
              mkError value
                =  fail
                $  "expected "
                <> show expectation
                <> " but received "
                <> show value
                <> " for conversion from "
                <> show source

mkConvertEither :: Show error => (a -> Either error b) -> (a -> Either ConversionError b)
mkConvertEither f = left (ConversionError . show) . f

typeName :: forall (a :: Type) . Typeable a => String
typeName
  = fromMaybe (Err.error "GHC error! invalid type name")
  . List.stripPrefix "Proxy * "
  . show
  $ typeOf (Proxy @a)
