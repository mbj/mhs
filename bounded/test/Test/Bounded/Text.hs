{-# OPTIONS_GHC -fdefer-type-errors #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}

module Test.Bounded.Text where

import Control.Exception (ErrorCall)
import Data.Bounded.Prelude
import Data.Bounded.Text
import Data.Bounded.TypeLevel
import Test.HUnit
import Test.JSON
import Test.ShouldNotTypecheck
import Test.Tasty
import Test.Tasty.HUnit
import Test.TypeSpec (Expect, ShouldBe, TypeSpec(..))
import Test.TypedSpec

type Example  = BoundText' "Example" '(2, 2)
type Example0 = BoundText' "Example0" '(0, 2)

testTree :: TestTree
testTree = testGroup "BoundText Tests"
  [ typeSpec
  , valueSpec
  ]

valueSpec :: TestTree
valueSpec = testGroup "BoundText value tests"
  [ acceptsValidJson
  , acceptsValidJson0
  , invalidExamples
  , rejectsInvalidJson
  , truncateExamples
  , validExamples
  ]
  where
    acceptsValidJson0 :: TestTree
    acceptsValidJson0 = testGroup "Accepts bound JSON 0 values"
      [ acceptJSONText ("", fromType @"" @Example0)
      ]

    acceptsValidJson :: TestTree
    acceptsValidJson = testGroup "Accepts bound JSON values"
      [ acceptJSONText ("CA", fromType @"CA" @Example)
      , acceptJSONText ("OR", fromType @"OR" @Example)
      ]

    rejectsInvalidJson :: TestTree
    rejectsInvalidJson = testGroup "Rejects out of bound JSON values"
      [ rejectJSONText @Example ("", "parsing Example failed, actual length: 0 needs to be within min: 2 and max: 2")
      , rejectJSONText @Example ("Foo", "parsing Example failed, actual length: 3 needs to be within min: 2 and max: 2")
      ]

    invalidExamples :: TestTree
    invalidExamples =  testCase "Throws errors for out of bound values" $ do
      shouldFail "Foo"
      shouldFail "F"
      where
        shouldFail :: Text -> Assertion
        shouldFail value
          = shouldFailWith @ErrorCall
            "length should have been between"
            (mkExample value @?= mkExample value)

    validExamples :: TestTree
    validExamples = testCase "Creates values for correct input" $ do
      fromType @"CA" @?= mkExample "CA"
      fromType @"OR" @?= mkExample "OR"

    truncateExamples :: TestTree
    truncateExamples = testCase "Truncates when requested" $ do
      convertTruncate @1 @2 @"label" "ab"  @?= pure (fromType @"ab")
      convertTruncate @1 @2 @"label" "abc" @?= pure (fromType @"ab")
      convertTruncate @1 @2 @"label" ""    @?= empty

mkExample :: Text -> Example
mkExample = convertImpure

type IsValidExample text
  = ShouldBe (IsInRange (Length text) 2 2) (() :: Constraint, () :: Constraint)

typeSpec :: TestTree
typeSpec = testGroup "Type checks correctly"
  [ accepted
  , denied
  ]
  where
    accepted :: TestTree
    accepted = typeTestCase "Type checks for valid examples" validExamples
      where
        validExamples
          :: Expect '[ IsValidExample "CA"
                     , IsValidExample "OR"
                     ]
        validExamples = Valid

    denied :: TestTree
    denied
      = testCase "Doesn't type check for invalid examples"
      $ shouldNotTypecheck invalidExamples
      where
        invalidExamples
          :: Expect '[ IsValidExample "Foo"
                     , IsValidExample "F"
                     ]
        invalidExamples = Invalid
