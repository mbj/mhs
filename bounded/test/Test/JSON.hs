module Test.JSON where

import Data.Bounded.Prelude
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Aeson       as JSON
import qualified Data.Aeson.Types as JSON

acceptJSONText
  :: forall a . (Eq a, JSON.FromJSON a, JSON.ToJSON a, Show a)
  => (Text, a)
  -> TestTree
acceptJSONText (input, expected) = acceptJSON (JSON.String input, expected)

rejectJSONText
  :: forall a. (Eq a, Show a, JSON.FromJSON a)
  => (Text, Text)
  -> TestTree
rejectJSONText (input, message)
  = rejectJSON @a (JSON.String input, message)

acceptJSON
  :: forall a . (Eq a, JSON.FromJSON a, JSON.ToJSON a, Show a)
  => (JSON.Value, a)
  -> TestTree
acceptJSON (input, expected) = do
  let actual :: Either String a = JSON.parseEither JSON.parseJSON input

  testGroup ("for: " <> show input)
    [ testCase "parse expected" $ pure expected @=? actual
    , testCase "round trip" $ either
      (const $ assertBool "parse failure" True)
      (assertEqual "" input . JSON.toJSON)
      actual
    ]

rejectJSON
  :: forall a. (Eq a, Show a, JSON.FromJSON a)
  => (JSON.Value, Text)
  -> TestTree
rejectJSON (input, message)
  = testCase ("rejects: " <> show input)
  $ Left (convertText $ "Error in $: " <> message) @=? JSON.parseEither (JSON.parseJSON @a) input
