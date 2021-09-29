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
import MPrelude                  as Exports
import MRIO.Core                 as Exports
import qualified Data.ByteString as BS

encodeUtf8 :: Conversion Text a => a -> BS.ByteString
encodeUtf8 = convert . convert @Text

safeHead :: [a] -> Maybe a
safeHead = \case
  (x:_) -> pure x
  _      -> empty

showc :: forall a b . (Show a, Conversion b String) => a -> b
showc = convert . show
