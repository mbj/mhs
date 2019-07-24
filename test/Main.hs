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
      , ("/1",           Paths.Template [Paths.Static "1"])
      , ("/__foo__",     Paths.Template [Paths.Static "__foo__"])
      , ("/foo",         Paths.Template [Paths.Static "foo"])
      , ("/foo/bar",     Paths.Template [Paths.Static "foo", Paths.Static "bar"])
      , ("/foo/{bar}",   Paths.Template [Paths.Static "foo", Paths.Dynamic "bar"])
      , ("/foo_bar",     Paths.Template [Paths.Static "foo_bar"])
      , ("/{foo}",       Paths.Template [Paths.Dynamic "foo"])
      , ("/{foo}/bar",   Paths.Template [Paths.Dynamic "foo", Paths.Static "bar"])
      , ("/{foo}/{bar}", Paths.Template [Paths.Dynamic "foo", Paths.Dynamic "bar"])
      ]

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
