{-# LANGUAGE TypeApplications #-}

import MPrelude
import System.IO (IO)
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Aeson                as JSON
import qualified Data.Aeson.Types          as JSON
import qualified Data.Map.Strict           as Map
import qualified Network.HTTP.Types.Status as HTTP
import qualified OpenApi.Paths             as Paths

suite :: TestTree
suite = testGroup "Test Suite" [parseResponses, parseTemplate]

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
    rejected = mkRejected' <$>
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

    mkRejected' :: Text -> TestTree
    mkRejected' input =
      mkRejected
        (JSON.parseJSON @Paths.Template)
        "Error in $: invalid template path:"
        (JSON.toJSON input, convertText $ show input)

parseResponses :: TestTree
  = testGroup "OpenAPI.Path.Responses parsing"
  $ accepted <> rejected
  where
    accepted :: [TestTree]
    accepted = mkAccepted <$>
      [ (JSON.object empty, Paths.Responses { default' = empty, patterns = Map.empty })
      , (responsesJSON, responses)
      ]

    responses :: Paths.Responses
    responses = Paths.Responses {default' = empty, ..}
      where
        patterns = Map.fromList
          [ (Paths.ResponseStatusExact HTTP.status200, response) ]

        response = Paths.Response
          { description = empty
          , headers     = empty
          , content     = Map.empty
          }

    responsesJSON :: JSON.Value
    responsesJSON =
      JSON.object
        [ ("200", JSON.object [("content", JSON.object empty)]) ]

    rejected :: [TestTree]
    rejected = mkRejected (JSON.parseJSON @Paths.Responses) "Error in $:" <$>
      [ ("", "expected responses, encountered String")
      , (JSON.object [("", JSON.object empty)], "Invalid status code pattern: \"\"")
      , (JSON.object [("-1", JSON.object empty)], "Invalid status code pattern: \"-1\"")
      , (JSON.object [("0", JSON.object empty)], "Invalid status code pattern: \"0\"")
      , (JSON.object [("512", JSON.object empty)], "Invalid status code pattern: \"512\"")
      , (JSON.object [("99", JSON.object empty)], "Invalid status code pattern: \"99\"")
      , (JSON.object [("foo", JSON.object empty)], "Invalid status code pattern: \"foo\"")
      ]

main :: IO ()
main = defaultMain suite

mkAccepted
  :: (Eq a, JSON.FromJSON a, Show a)
  => (JSON.Value, a)
  -> TestTree
mkAccepted (input, expected)
  = testCase (convertText $ "accepts: " <> show input)
  $ pure expected @=? JSON.parseEither JSON.parseJSON input

mkRejected
  :: (Eq a, Show a)
  => (JSON.Value -> JSON.Parser a)
  -> Text
  -> (JSON.Value, Text)
  -> TestTree
mkRejected parseJSON prefix (input, message)
  = testCase (convertText $ "rejects: " <> show input)
  $ Left (convertText $ prefix <> " " <> message) @=? JSON.parseEither parseJSON input
