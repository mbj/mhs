module MPrelude (module Exports, identity) where

import Control.Applicative as Exports
  ( Alternative
  , Applicative
  , (<*>)
  , (<|>)
  , empty
  , pure
  )

import Control.Monad as Exports
  ( Monad
  , (<=<)
  , (=<<)
  , (>>)
  , void
  )

import Control.Monad.Fail as Exports
  ( MonadFail
  , fail
  )

import Control.Monad.IO.Class as Exports
  ( MonadIO
  , liftIO
  )

import Data.Bool as Exports
  ( Bool(..)
  , (&&)
  , (||)
  , not
  , otherwise
  )

import Data.Either as Exports
  ( Either(Left, Right)
  , either
  )

import Data.Eq as Exports
  ( Eq
  , (/=)
  , (==)
  )

import Data.Foldable as Exports
  ( Foldable
  )

import Data.Function as Exports
  ( ($)
  , (.)
  , const
  )

import Data.Functor as Exports
  ( Functor
  , (<$>)
  )

import Data.Maybe as Exports
  ( Maybe(Just)
  , fromMaybe
  , listToMaybe
  , maybe
  )

import Data.Monoid as Exports
  ( Monoid
  , mempty
  )

import Data.Ord as Exports
  ( Ord
  , (<)
  , (<=)
  , (>)
  , (>=)
  )

import Data.Semigroup as Exports
  ( Semigroup
  , (<>)
  )

import Data.Text as Exports
  ( Text
  )

import Data.Text.Conversions as Exports
  ( ToText
  , convertText
  , toText
  )

import Data.Tuple as Exports
  ( uncurry
  )

import Text.Show as Exports
  ( Show
  , show
  )

identity :: a -> a
identity value = value
