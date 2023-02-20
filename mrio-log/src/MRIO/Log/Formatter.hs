{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds           #-}

module MRIO.Log.Formatter where

import Data.Conversions
import Data.Function
import Data.List.NonEmpty (NonEmpty((:|)))
import GHC.Records (HasField, getField)
import MPrelude
import MRIO.Log.Types

import qualified Data.Foldable            as Foldable
import qualified Data.List.NonEmpty       as NonEmpty
import qualified Data.Text.Lazy.Builder   as Text
import qualified Data.Time.Format.ISO8601 as ISO8601

newtype Formatter = Formatter (NonEmpty (Message -> Text.Builder))

instance Semigroup Formatter where
  (Formatter itemsA) <> (Formatter itemsB) = Formatter $ itemsA <> itemsB

render :: Formatter -> Message -> Text
render (Formatter items) message
  = convert
  . Text.toLazyText
  . Foldable.fold
  $ NonEmpty.intersperse " " (($ message) <$> items)

defaultCLIFormatter :: Formatter
defaultCLIFormatter
  =  fSeverity
  <> fISO8601Time
  <> fThreadId
  <> fMessage

fISO8601Time :: Formatter
fISO8601Time = formatField @"time" $  brackets . Text.fromString . ISO8601.iso8601Show

fThreadId :: Formatter
fThreadId = formatField @"threadId" $ brackets . Text.fromString . show

fSeverity :: Formatter
fSeverity = formatField @"severity" $ \case
  Debug -> "[Debug]"
  Info  -> "[Info] "
  Warn  -> "[Warn] "
  Error -> "[Error]"

fMessage :: Formatter
fMessage = formatField @"message" Text.fromText

formatField :: forall field a . HasField field Message a => (a -> Text.Builder) -> Formatter
formatField format = Formatter $ (:| []) (format . getField @field)

brackets :: Text.Builder -> Text.Builder
brackets builder = Text.singleton '[' <> builder <> Text.singleton ']'
