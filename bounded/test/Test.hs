module Main
  ( main
  )
where

import Data.Bounded.Prelude
import Test.Tasty

import qualified Devtools
import qualified Test.Bounded.Integral as BoundNumber
import qualified Test.Bounded.Text     as BoundText

main :: IO ()
main
  = defaultMain
  $ testGroup "bounded"
  [ BoundNumber.testTree
  , BoundText.testTree
  , Devtools.testTree $$(Devtools.readDependencies [Devtools.Target "bounded"])
  ]
