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
main = do
  devtools <- Devtools.testTree Devtools.defaultConfig

  defaultMain $
    testGroup
      "bounded"
        [ BoundNumber.testTree
        , BoundText.testTree
        , devtools
        ]
