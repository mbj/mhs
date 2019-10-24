{-# LANGUAGE TypeApplications #-}

import Data.HashMap.Strict (HashMap)
import Data.Maybe (catMaybes)
import Data.String (String)
import MPrelude
import System.IO (IO)
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Aeson                as JSON
import qualified Data.Aeson.Types          as JSON
import qualified Data.HashMap.Strict       as HashMap
import qualified Data.Map.Strict           as Map
import qualified Network.HTTP.Types.Status as HTTP
import qualified OpenApi.Paths             as Paths
import qualified OpenApi.Schema            as Schema

suite :: TestTree
suite = testGroup "Test Suite" [parseFormat, parseResponses, parseTemplate]

parseFormat :: TestTree
parseFormat
  = testGroup "OpenAPI.Path.Format parsing"
  $ accepted <> rejected
  where
    accepted :: [TestTree]
    accepted = mkAccepted <$>
      [ ("decimal",   Schema.CustomFormat "decimal")
      , ("unix-time", Schema.UnixTime)
      ]

    rejected :: [TestTree]
    rejected = mkRejected (JSON.parseJSON @Schema.Format) "Error in $:" <$>
      [(JSON.object empty, "parsing format failed, expected String, but encountered Object")]

parseTemplate :: TestTree
parseTemplate
  = testGroup "OpenAPI.Path.Template parsing"
  $ accepted <> rejected
  where
    accepted :: [TestTree]
    accepted = mkAccepted <$>
      [ ("/",            Paths.Template empty)
      , ("/1",           Paths.Template [static' "1"])
      , ("/__foo__",     Paths.Template [static' "__foo__"])
      , ("/foo",         Paths.Template [static' "foo"])
      , ("/foo/bar",     Paths.Template [static' "foo", static' "bar"])
      , ("/foo/{bar}",   Paths.Template [static' "foo", dynamic "bar"])
      , ("/foo_bar",     Paths.Template [static' "foo_bar"])
      , ("/{foo}",       Paths.Template [dynamic "foo"])
      , ("/{foo}/bar",   Paths.Template [dynamic "foo", static' "bar"])
      , ("/{foo}/{bar}", Paths.Template [dynamic "foo", dynamic "bar"])
      ]

    dynamic = Paths.Dynamic . Paths.ParameterName

    static' = Paths.Static

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
      [ ("", "parsing responses failed, expected Object, but encountered String")
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
  :: forall a . (Eq a, JSON.FromJSON a, JSON.ToJSON a, Show a)
  => (JSON.Value, a)
  -> TestTree
mkAccepted (input, expected) = do
  let actual :: Either String a = JSON.parseEither JSON.parseJSON input

  testGroup ("for: " <> show input)
    [ testCase "parse expected" $ pure expected @=? actual
    , testCase "round trip" $ either
      (const $ assertBool "parse failure" True)
      (assertEqual "" input . normalize . JSON.toJSON)
      actual
    ]
  where
    normalize :: JSON.Value -> JSON.Value
    normalize = \case
      JSON.Array array ->
        JSON.Array $ normalize <$> array
      JSON.Object properties ->
        JSON.Object $ normalizeAttributes properties
      other -> other

    normalizeAttributes :: HashMap Text JSON.Value -> HashMap Text JSON.Value
    normalizeAttributes map
      = HashMap.fromList . catMaybes $ normalizePair <$> HashMap.toList map

    normalizePair :: (Text, JSON.Value) -> Maybe (Text, JSON.Value)
    normalizePair (key, value) =
      if value == JSON.Null
         then empty
         else pure (key, normalize value)

mkRejected
  :: (Eq a, Show a)
  => (JSON.Value -> JSON.Parser a)
  -> Text
  -> (JSON.Value, Text)
  -> TestTree
mkRejected parseJSON prefix (input, message)
  = testCase (convertText $ "rejects: " <> show input)
  $ Left (convertText $ prefix <> " " <> message) @=? JSON.parseEither parseJSON input
