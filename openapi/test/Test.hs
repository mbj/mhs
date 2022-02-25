import Data.HashMap.Strict (HashMap)
import Data.Maybe (catMaybes)
import OpenApi.Paths
import OpenApi.Prelude
import OpenApi.ReferenceOr
import OpenApi.Response
import OpenApi.Responses
import OpenApi.Schema
import OpenApi.TaggedText
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Aeson                as JSON
import qualified Data.Aeson.Types          as JSON
import qualified Data.HashMap.Strict       as HashMap
import qualified Data.Map.Strict           as Map
import qualified Devtools
import qualified Network.HTTP.Types.Status as HTTP

main :: IO ()
main = do
  devtools <- Devtools.testTree Devtools.defaultConfig
    { Devtools.hlintArguments = ["-XTypeApplications"]
    , Devtools.targets        = [Devtools.Target "openapi"]
    }

  defaultMain $ testGroup "openapi" [suite, devtools]

suite :: TestTree
suite = testGroup "Test Suite"
  [ parseFormat
  , parseResponses
  , parsePathTemplate
  ]

parseFormat :: TestTree
parseFormat
  = testGroup "OpenAPI.Path.Format parsing"
  $ accepted <> rejected
  where
    accepted :: [TestTree]
    accepted = mkAccepted <$>
      [ ("decimal",   CustomFormat "decimal")
      , ("unix-time", UnixTime)
      ]

    rejected :: [TestTree]
    rejected = mkRejected (JSON.parseJSON @Format) "Error in $:" <$>
      [(JSON.object empty, "parsing format failed, expected String, but encountered Object")]

parsePathTemplate :: TestTree
parsePathTemplate
  = testGroup "OpenAPI.Paths.PathTemplate parsing"
  $ accepted <> rejected
  where
    accepted :: [TestTree]
    accepted = mkAccepted <$>
      [ ("/",            PathTemplate empty)
      , ("/1",           PathTemplate [static' "1"])
      , ("/__foo__",     PathTemplate [static' "__foo__"])
      , ("/foo",         PathTemplate [static' "foo"])
      , ("/foo.csv",     PathTemplate [static' "foo.csv"])
      , ("/foo..csv",    PathTemplate [static' "foo..csv"])
      , ("/foo-bar",     PathTemplate [static' "foo-bar"])
      , ("/foo/bar",     PathTemplate [static' "foo", static' "bar"])
      , ("/foo/{bar}",   PathTemplate [static' "foo", dynamic "bar"])
      , ("/foo_bar",     PathTemplate [static' "foo_bar"])
      , ("/{foo}",       PathTemplate [dynamic "foo"])
      , ("/{foo}/bar",   PathTemplate [dynamic "foo", static' "bar"])
      , ("/{foo}/{bar}", PathTemplate [dynamic "foo", dynamic "bar"])
      ]

    dynamic = PathSegmentDynamic . TaggedText

    static' = PathSegmentStatic

    rejected :: [TestTree]
    rejected = mkRejected' <$>
      [ ""
      , "foo"
      , "//"
      , ".csv"
      , "/foo/"
      , "/foo/{id}/"
      , "/foo/{}"
      , "/{id}/"
      , "{id}"
      , "{}"
      , "{"
      , "}"
      ]

    mkRejected' :: Text -> TestTree
    mkRejected' input =
      mkRejected
        (JSON.parseJSON @PathTemplate)
        "Error in $: invalid template path:"
        (JSON.toJSON input, convertText $ show input)

parseResponses :: TestTree
parseResponses
  = testGroup "OpenAPI.Path.Responses parsing"
  $ accepted <> rejected
  where
    accepted :: [TestTree]
    accepted = mkAccepted <$>
      [ (JSON.object empty, Responses { default' = empty, patterns = Map.empty })
      , (responsesJSON, responses)
      ]

    responses :: Responses
    responses = Responses {default' = empty, ..}
      where
        patterns :: Map ResponseStatusPattern (ReferenceOr Response)
        patterns = [ (ResponseStatusExact HTTP.status200, Literal response) ]

        response = Response
          { description = TaggedText "some-description"
          , headers     = empty
          , content     = pure Map.empty
          }

    responsesJSON :: JSON.Value
    responsesJSON =
      JSON.object
        [ ("200", JSON.object [("content", JSON.object empty), ("description", "some-description")]) ]

    rejected :: [TestTree]
    rejected = mkRejected (JSON.parseJSON @Responses) "Error in $:" <$>
      [ ("", "parsing responses failed, expected Object, but encountered String")
      , (JSON.object [("", JSON.object empty)], "Invalid status code pattern: \"\"")
      , (JSON.object [("-1", JSON.object empty)], "Invalid status code pattern: \"-1\"")
      , (JSON.object [("0", JSON.object empty)], "Invalid status code pattern: \"0\"")
      , (JSON.object [("512", JSON.object empty)], "Invalid status code pattern: \"512\"")
      , (JSON.object [("99", JSON.object empty)], "Invalid status code pattern: \"99\"")
      , (JSON.object [("foo", JSON.object empty)], "Invalid status code pattern: \"foo\"")
      ]

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
