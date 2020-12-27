-- | Tasty provider for TypeSpec
-- TODO: extract into package see https://github.com/sheyll/type-spec/issues/2
-- and possibly add typeTestCaseInfo & typeTestCaseSteps functions
--
module Test.TypedSpec where

import Data.Bounded.Prelude
import Test.Tasty.Providers
import Test.TypeSpec.Core

newtype TypeSpecTasty expectation = TypeSpecTasty (TypeSpec expectation)
  deriving newtype Show
  deriving stock Typeable

instance (PrettyTypeSpec a, Typeable (TypeSpecTasty a)) => IsTest (TypeSpecTasty a) where
  run _ (TypeSpecTasty assertion) _ = pure $ case assertion of
    Valid   -> testPassed (show assertion)
    Invalid -> testFailed (show assertion)

  testOptions = return []

typeTestCase
  :: (PrettyTypeSpec expectation, Typeable (TypeSpecTasty expectation))
  => TestName
  -> TypeSpec expectation
  -> TestTree
typeTestCase name = singleTest name . TypeSpecTasty
