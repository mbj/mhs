import Data.String (String)
import PGT.CLI
import PGT.Prelude
import System.Exit (ExitCode(ExitFailure))
import System.IO (IO)
import Test.Hspec(context, describe, hspec, it, shouldBe, shouldThrow)

import qualified Data.Foldable as Foldable

-- | Test entry point
main :: IO ()
main = hspec .
  describe "pgt test" $ do
    context "on success example" .
      it "returns success" $ do
        result <- runTest (["spec/success.sql"] :: [String])
        result `shouldBe` ()
    context "on only failure example" .
      it "returns error" $
        runTest (["spec/failure.sql"] :: [String]) `shouldThrow` (== ExitFailure 1)
    context "on failure example" .
      it "returns error" $
        runTest (["spec/success.sql", "spec/failure.sql"] :: [String]) `shouldThrow` (== ExitFailure 1)

runTest :: Foldable f => f String -> IO ()
runTest files = run $ ["--silent", "test", "--"] <> Foldable.toList files
