import Data.String (String)
import MPrelude
import System.IO (IO)
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Aeson       as JSON
import qualified Data.Aeson.Types as JSON
import qualified OpenApi.Paths    as Paths

suite :: TestTree
suite = testGroup "Test Suite" [parseTemplate]

parseTemplate :: TestTree
parseTemplate
  = testGroup "OpenAPI.Path.Template parsing"
  $ accepted <> rejected
  where
    accepted :: [TestTree]
    accepted = mkAccepted <$>
      [ ("/",            Paths.Template empty)
      , ("/1",           Paths.Template [static "1"])
      , ("/__foo__",     Paths.Template [static "__foo__"])
      , ("/foo",         Paths.Template [static "foo"])
      , ("/foo/bar",     Paths.Template [static "foo", static "bar"])
      , ("/foo/{bar}",   Paths.Template [static "foo", dynamic "bar"])
      , ("/foo_bar",     Paths.Template [static "foo_bar"])
      , ("/{foo}",       Paths.Template [dynamic "foo"])
      , ("/{foo}/bar",   Paths.Template [dynamic "foo", static "bar"])
      , ("/{foo}/{bar}", Paths.Template [dynamic "foo", dynamic "bar"])
      ]

    dynamic = Paths.Dynamic . Paths.ParameterName

    static = Paths.Static

    rejected :: [TestTree]
    rejected = mkRejected <$>
      [ ""
      , "foo"
      , "//"
      , "/foo/"
      , "/foo/{id}/"
      , "/foo/{}"
      , "/{id}/"
      , "{id}"
      , "{}"
      ]

    mkAccepted :: (Text, Paths.Template) -> TestTree
    mkAccepted (input, expected)
      = testCase (convertText ("accepts: " <> show input))
      $ pure expected @=? parseJSON input

    mkRejected :: Text -> TestTree
    mkRejected input
      = testCase (convertText ("rejects: " <> show input))
      $ Left ("Error in $: invalid template path: " <> show input) @=? parseJSON input

    parseJSON :: Text -> Either String Paths.Template
    parseJSON = JSON.parseEither JSON.parseJSON . JSON.String

main :: IO ()
main = defaultMain suite
