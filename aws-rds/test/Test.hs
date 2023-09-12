{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import MPrelude

import qualified Devtools
import qualified Test.Tasty as Tasty

main :: IO ()
main =
  liftIO
    . Tasty.defaultMain
    $ Devtools.testTree $$(Devtools.readDependencies [Devtools.Target "aws-rds"])
