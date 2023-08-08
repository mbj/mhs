{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import AWS.TemporaryIngressRule.Prelude

import qualified Devtools
import qualified Test.Tasty as Tasty

main :: IO ()
main =
  liftIO
    . Tasty.defaultMain
    $ Devtools.testTree $$(Devtools.readDependencies [Devtools.Target "aws-temporary-ingress-rule"])
