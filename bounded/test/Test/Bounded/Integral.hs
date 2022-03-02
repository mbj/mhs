{-# OPTIONS_GHC -fdefer-type-errors #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}

module Test.Bounded.Integral (testTree) where

import Control.Exception (ErrorCall)
import Data.Bounded.Integral
import Data.Bounded.Prelude
import Data.Bounded.TypeLevel
import Test.HUnit
import Test.JSON
import Test.ShouldNotTypecheck
import Test.Tasty
import Test.Tasty.HUnit
import Test.TypeSpec (Expect, ShouldBe, TypeSpec(..))
import Test.TypedSpec

import qualified Data.Aeson.Types as JSON

type Example = BoundNumber "Example" '(1, 5)

testTree :: TestTree
testTree = testGroup "BoundNumber Tests"
  [ typeSpec
  , valueSpec
  ]

valueSpec :: TestTree
valueSpec = testGroup "BoundNumber value tests"
  [ acceptsValidJson
  , invalidExamples
  , maybeExamples
  , rejectsInvalidJson
  , validExamples
  ]
  where
    acceptsValidJson :: TestTree
    acceptsValidJson = testGroup "Accepts bound JSON values"
      [ acceptJSON (JSON.Number 1, fromType @1 @Example)
      , acceptJSON (JSON.Number 5, fromType @5 @Example)
      ]

    rejectsInvalidJson :: TestTree
    rejectsInvalidJson = testGroup "Rejects out of bound JSON values"
      [ rejectJSON @Example (JSON.Number 0, "parsing Example failed, cannot be less than 1")
      , rejectJSON @Example (JSON.Number 6, "parsing Example failed, cannot be greater than 5")
      ]

    maybeExamples :: TestTree
    maybeExamples = testCase "Create empty value for invalids and non-empty for valid values" $ do
      convertMaybe @Example @Natural 0 @?= empty
      convertMaybe @Example @Natural 6 @?= empty
      convertMaybe @Example @Natural 1 @?= pure (mkExample 1)
      convertMaybe @Example @Natural 5 @?= pure (mkExample 5)

    invalidExamples :: TestTree
    invalidExamples =  testCase "Throws errors for out of bound values" $ do
      shouldFail 6
      shouldFail 0
      where
        expectedError :: Natural -> String
        expectedError nat = boundError @Natural @Example nat minBound maxBound

        shouldFail :: Natural -> Assertion
        shouldFail value
          = shouldFailWith @ErrorCall
            (expectedError value)
            (mkExample value @?= mkExample value)

    validExamples :: TestTree
    validExamples = testCase "Creates values for correct input" $ do
      fmap (convert @Natural @Example) examples @?= exampleRange
      fromType @5 @?= mkExample 5
      fromType @1 @?= mkExample 1
      where
        exampleRange :: [Natural]
        exampleRange = [1 .. 5]

        examples :: [Example]
        examples = mkExample <$> exampleRange

mkExample :: Natural -> Example
mkExample = convertImpure

type IsValidExample nat
  = ShouldBe (IsInRange nat 1 5) (() :: Constraint, () :: Constraint)

typeSpec :: TestTree
typeSpec = testGroup "Type-checks correctly"
  [ accepted
  , denied
  ]
  where
    accepted :: TestTree
    accepted = typeTestCase "Numbers in range 1 to 5 should be valid examples" validExamples
      where
        validExamples
          :: Expect '[ IsValidExample 1
                     , IsValidExample 2
                     , IsValidExample 3
                     , IsValidExample 4
                     , IsValidExample 5
                     ]
        validExamples = Valid

    denied :: TestTree
    denied
      = testCase "Doesn't type-check for out of bound values"
      $ shouldNotTypecheck invalidExamples
      where
        invalidExamples
          :: Expect '[ IsValidExample 6
                     , IsValidExample 0
                     ]
        invalidExamples = Invalid
