module OpenApi.JSON
  ( generateRenamed
  , genericParseJSON
  , parseJSONFixed
  , parseRefSum
  , parseRenamed
  )
where

import Data.Map.Strict (Map)
import Data.String (String)
import GHC.Enum (Bounded, Enum, minBound)
import GHC.Generics (Generic, Rep)
import OpenApi.Prelude

import qualified Data.Aeson.Types    as JSON
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List           as List
import qualified Data.Map.Strict     as Map
import qualified Data.Text           as Text

parseRenamed
  :: (Generic a, JSON.GFromJSON JSON.Zero (Rep a))
  => Map String String
  -> JSON.Value
  -> JSON.Parser a
parseRenamed = JSON.genericParseJSON . renameOptions

genericParseJSON
  :: ((JSON.GFromJSON JSON.Zero (Rep a)), Generic a)
  => JSON.Value
  -> JSON.Parser a
genericParseJSON = JSON.genericParseJSON defaultOptions

generateRenamed
  :: (Generic a, JSON.GToJSON JSON.Zero (Rep a))
  => Map String String
  -> a
  -> JSON.Value
generateRenamed = JSON.genericToJSON . renameOptions

parseJSONFixed
  :: forall a b c . (Bounded b, Enum b, Eq a, Show a)
  => Text
  -> (String -> (a -> JSON.Parser b) -> c -> JSON.Parser b)
  -> (b -> a)
  -> c
  -> JSON.Parser b
parseJSONFixed name withValue map =
  withValue (convertText name) $ \value ->
    maybe (unexpected value) pure $ List.lookup value options
  where
    options :: [(a, b)]
    options = (\item -> (map item, item)) <$> [minBound..]

    unexpected :: a -> JSON.Parser b
    unexpected value
      = fail
      . convertText
      $ "Unexpected " <> name <> ": " <> convertText (show value)

parseRefSum
  :: forall a b c . (JSON.FromJSON a)
  => (Text -> b)
  -> (a -> c)
  -> (b -> c)
  -> Text
  -> Text
  -> JSON.Value
  -> JSON.Parser c
parseRefSum mkName mkBody mkRef prefix name value =
  JSON.withObject (convertText name) parseObject value
  where
    parseObject object =
      maybe parseBody (tryRef object) $ HashMap.lookup "$ref" object

    parseReference :: Text -> JSON.Parser c
    parseReference exp =
      if prefix `Text.isPrefixOf` exp
        then pure . mkRef . mkName $ Text.drop (Text.length prefix) exp
        else fail $ "Invalid reference for " <> show name <> ": " <> show exp

    parseBody = mkBody <$> JSON.parseJSON value

    tryRef :: JSON.Object -> JSON.Value -> JSON.Parser c
    tryRef object =
      JSON.withText "ref expression" $ \expression ->
        if HashMap.keys object == ["$ref"]
           then parseReference expression
           else fail "$ref with more than one key"

defaultOptions :: JSON.Options
defaultOptions = JSON.defaultOptions { JSON.rejectUnknownFields = True }

renameOptions :: Map String String -> JSON.Options
renameOptions renames =
  defaultOptions { JSON.fieldLabelModifier = rename }
    where
      rename input = fromMaybe input $ Map.lookup input renames
