{-# LANGUAGE DerivingStrategies #-}

module MissingDerivingStrategy where

import Data.Eq (Eq)

data Foo = Foo deriving Eq

data Bar = Bar deriving stock Eq
