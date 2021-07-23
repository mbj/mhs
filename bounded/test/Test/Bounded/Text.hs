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

type Example = BoundText' "Example" '(2, 2)

testTree :: TestTree
testTree = testGroup "BoundText Tests"
  [ typeSpec
  , valueSpec
  ]

valueSpec :: TestTree
valueSpec = testGroup "BoundText value tests"
  [ acceptsValidJson
  , invalidExamples
  , rejectsInvalidJson
  , validExamples
  ]
  where
    acceptsValidJson :: TestTree
    acceptsValidJson = testGroup "Accepts bound JSON values"
      [ acceptJSONText ("CA", fromType @"CA" @Example)
      , acceptJSONText ("OR", fromType @"OR" @Example)
      ]

    rejectsInvalidJson :: TestTree
    rejectsInvalidJson = testGroup "Rejects out of bound JSON values"
      [ rejectJSONText @Example (mempty, "parsing Example failed, cannot be empty String")
      , rejectJSONText @Example ("Foo", "parsing Example failed, cannot be longer than 2 characters")
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


mkExample :: Text -> Example
mkExample = convertUnsafe

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
