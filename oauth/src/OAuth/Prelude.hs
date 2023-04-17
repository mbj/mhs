module OAuth.Prelude
  ( module Exports
  , encodeUtf8
  , safeHead
  , showc
  )
where

import Control.Monad.Reader      as Exports (asks)
import Data.Conversions          as Exports
import GHC.Records               as Exports (HasField(..))
import MIO.Core                  as Exports
import MPrelude                  as Exports

import qualified Data.ByteString    as BS
import qualified Data.Text.Encoding as Text

encodeUtf8 :: Conversion Text a => a -> BS.ByteString
encodeUtf8 = Text.encodeUtf8 . convert @Text

safeHead :: [a] -> Maybe a
safeHead = \case
  (x:_) -> pure x
  _      -> empty

showc :: forall a b . (Show a, Conversion b String) => a -> b
showc = convert . show
