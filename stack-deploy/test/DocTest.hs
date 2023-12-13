module Main where

import MPrelude

import qualified Test.DocTest

main :: IO ()
main = Test.DocTest.mainFromCabal "stack-deploy" ["--no-implicit-module-import"]
