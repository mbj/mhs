module Devtools.HLint (testTree) where

import Control.Applicative (empty, pure)
import Data.Function (($), (.), const)
import Data.Functor (void)
import Data.Semigroup ((<>))
import Data.String (String)
import Data.Typeable (Typeable)
import System.IO

import qualified Data.Foldable                      as Foldable
import qualified Language.Haskell.HLint4            as HLint
import qualified System.Console.CmdArgs.Verbosity   as CmdArgs
import qualified Test.Tasty                         as Tasty
import qualified Test.Tasty.Providers               as Tasty
import qualified Test.Tasty.Providers.ConsoleFormat as Tasty

newtype HLintTest = HLintTest [String]
  deriving stock Typeable

instance Tasty.IsTest HLintTest where
  run _options (HLintTest arguments) _callback = runHLintTest arguments
  testOptions = pure empty

testTree :: [String] -> Tasty.TestTree
testTree = Tasty.singleTest "hlint" . HLintTest

runHLintTest :: [String] -> IO Tasty.Result
runHLintTest arguments = do
  ideas <- HLint.hlint $ ["--quiet"] <> arguments <> ["."]

  pure $ if Foldable.null ideas
    then Tasty.testPassed empty
    else Tasty.testFailedDetails empty
      . Tasty.ResultDetailsPrinter
      . const
      . const
      $ runHLintVerbose arguments

-- Run HLint (again) but with output enabled.
-- There is no good public API in HLint to render the output.
runHLintVerbose :: [String] -> IO ()
runHLintVerbose arguments = do
  -- CmdArgs the CLI parsing lib for hlint leaks global state.
  -- We have to reset it here.
  CmdArgs.setVerbosity CmdArgs.Normal
  void . HLint.hlint $ arguments <> ["--", "."]
