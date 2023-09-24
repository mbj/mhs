{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import MPrelude

import qualified AWS.RDS
import qualified Data.Aeson               as JSON
import qualified Data.Aeson.Encode.Pretty as JSON
import qualified Data.Text.Lazy           as Text
import qualified Data.Text.Lazy.Builder   as Text
import qualified Devtools
import qualified Stratosphere             as CFT
import qualified Test.Tasty               as Tasty
import qualified Test.Tasty.MGolden       as Tasty

main :: IO ()
main
  = liftIO
  . Tasty.defaultMain
  $ Tasty.testGroup "aws-rds"
    [ Devtools.testTree $$(Devtools.readDependencies [Devtools.Target "mio-log"])
    , testPolicy
    ]

testPolicy :: Tasty.TestTree
testPolicy =
  Tasty.goldenTest
    "policy"
    "test/expected/policy.json"
    ( pure
    . Text.toStrict
    . Text.toLazyText
    . JSON.encodePrettyToTextBuilder
    . JSON.toJSON
    $ AWS.RDS.mkConnectPolicy (CFT.Literal "TestDatabaseLogicalName") (CFT.Literal "test_db_user_name")
    )
