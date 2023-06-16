{-# LANGUAGE TemplateHaskell #-}

module Main
  ( main
  )
where

import MPrelude

import qualified Devtools
import qualified Test.Data.Conversions.Integral
import qualified Test.Tasty                     as Tasty

main :: IO ()
main =
  liftIO . Tasty.defaultMain .
    Tasty.testGroup "Conversions" $
      [ Devtools.testTree $$(Devtools.readDependencies [Devtools.Target "conversions"])
      , Test.Data.Conversions.Integral.testTree
      ]
