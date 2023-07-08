module PGT.Shard
  ( ShardConfig(..)
  , ShardCount(..)
  , ShardIndex(..)
  , defaultShardCount
  , defaultShardIndex
  , parseShardConfig
  , parseShardCount
  , selectShard
  )
where

import Data.Vector (Vector)
import PGT.Prelude
import Prelude ((-), div, succ)

import qualified Data.Vector as Vector

newtype ShardCount = ShardCount Natural
  deriving stock (Eq, Show)

newtype ShardIndex = ShardIndex Natural
  deriving stock (Eq, Show)

defaultShardCount :: ShardCount
defaultShardCount = ShardCount 1

defaultShardIndex :: ShardIndex
defaultShardIndex = ShardIndex 0

parseShardCount :: Natural -> Either String ShardCount
parseShardCount = \case
  0 -> Left "Shard count cannot be 0"
  n -> pure $ ShardCount n

parseShardConfig :: ShardCount -> ShardIndex -> Either String ShardConfig
parseShardConfig count@(ShardCount countValue) index@(ShardIndex indexValue) =
  if indexValue >= countValue
    then Left "Shard index ouside of shard count"
    else pure ShardConfig{..}

selectShard :: ShardConfig -> Vector a -> Vector a
selectShard ShardConfig{count=(ShardCount count), index=(ShardIndex index)} items =
    Vector.slice (convertImpure startIndex) (convertImpure effectiveChunkSize) items
  where
    nominalChunkSize :: Natural
    nominalChunkSize = length `div` count

    startIndex :: Natural
    startIndex = index * nominalChunkSize

    effectiveChunkSize :: Natural
    effectiveChunkSize =
      if succ index == count
        then length - startIndex
        else nominalChunkSize

    length :: Natural
    length = convertImpure $ Vector.length items

data ShardConfig = ShardConfig
  { count :: ShardCount
  , index :: ShardIndex
  }
  deriving stock (Eq, Show)
