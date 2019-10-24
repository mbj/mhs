module Devtools (main, tests) where

import Control.Applicative (empty)
import System.IO (IO, putStrLn)

import qualified Devtools.Dependencies as Dependencies
import qualified Devtools.HLint        as HLint
import qualified Test.Tasty            as Tasty

main :: IO ()
main = do
  putStrLn empty
  Tasty.defaultMain tests

tests :: Tasty.TestTree
tests = Tasty.testGroup "devtools"
  [ Dependencies.testTree
  , HLint.testTree
  ]
