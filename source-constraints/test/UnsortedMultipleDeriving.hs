{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module UnorderedMultipleDeriving where

import Data.Eq (Eq)
import Text.Show (Show)

newtype SomeType = SomeType ()
  deriving stock Show
  deriving newtype Eq
