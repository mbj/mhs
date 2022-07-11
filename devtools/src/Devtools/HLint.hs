module Devtools.HLint (testTree) where

import Control.Applicative (empty, pure)
import Data.Function (($), (.), const)
import Data.Functor (void)
import Data.Typeable (Typeable)
import System.IO

import qualified Data.Foldable                      as Foldable
import qualified Language.Haskell.HLint             as HLint
import qualified System.Console.CmdArgs.Verbosity   as CmdArgs
import qualified Test.Tasty                         as Tasty
import qualified Test.Tasty.Providers               as Tasty
import qualified Test.Tasty.Providers.ConsoleFormat as Tasty

data HLintTest = HLintTest
  deriving stock Typeable

instance Tasty.IsTest HLintTest where
  run _options HLintTest _callback = runHLintTest
  testOptions = pure empty

testTree :: Tasty.TestTree
testTree = Tasty.singleTest "hlint" HLintTest

runHLintTest :: IO Tasty.Result
runHLintTest = do
  -- Super wasteful to run it twice, but have yet to find
  -- an way to buffer and only output "if" there are ideas.
  ideas <- HLint.hlint ["--threads", "--quiet", "."]

  pure $ if Foldable.null ideas
    then Tasty.testPassed empty
    else Tasty.testFailedDetails empty
      . Tasty.ResultDetailsPrinter
      . const
      . const
      $ runHLintVerbose

-- Run HLint (again) but with output enabled.
-- There is no good public API in HLint to render the output.
runHLintVerbose :: IO ()
runHLintVerbose = do
  -- CmdArgs the CLI parsing lib for hlint leaks global state.
  -- We have to reset it here.
  CmdArgs.setVerbosity CmdArgs.Normal
  void $ HLint.hlint ["--threads", "--", "."]
