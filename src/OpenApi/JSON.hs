module OpenApi.JSON
  ( generateRenamed
  , genericParseJSON
  , genericToJSON
  , parseJSONFixed
  , parseRenamed
  )
where

import GHC.Enum (Bounded, Enum, minBound)
import GHC.Generics (Rep)
import OpenApi.Prelude

import qualified Data.Aeson.Types    as JSON
import qualified Data.List           as List
import qualified Data.Map.Strict     as Map

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

genericToJSON
  :: (Generic a, JSON.GToJSON JSON.Zero (Rep a))
  => a
  -> JSON.Value
genericToJSON = JSON.genericToJSON defaultOptions

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

defaultOptions :: JSON.Options
defaultOptions = JSON.defaultOptions { JSON.rejectUnknownFields = True }

renameOptions :: Map String String -> JSON.Options
renameOptions renames =
  defaultOptions { JSON.fieldLabelModifier = rename }
    where
      rename input = fromMaybe input $ Map.lookup input renames
