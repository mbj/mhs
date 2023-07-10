module PGT.Test
  ( Test(..)
  , Tests
  )
where

import PGT.Prelude

import qualified System.Path as Path

data Test = Test
  { id   :: Natural
  , path :: Path.RelFile
  }
  deriving stock (Eq, Ord)

type Tests = Vector Test
