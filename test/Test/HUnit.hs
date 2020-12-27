module Test.HUnit where

import Data.Bounded.Prelude
import Test.Tasty.HUnit

import qualified Data.List as List

shouldFailWith :: forall e. Exception e => String -> Assertion -> Assertion
shouldFailWith msg assertion = do
  eResult <- try @e assertion
  case eResult of
    Right _            -> assertFailure "Error should have failed"
    Left (show -> err) -> unless (msg `List.isInfixOf` err)
      . assertFailure
      $ "Failed! expected error message: " <> err <> " but got " <> msg
