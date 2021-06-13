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
  , (>>=)
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

import Control.Monad.IO.Unlift  as Exports
  ( MonadUnliftIO
  )

import Data.Bool as Exports
  ( Bool(..)
  , (&&)
  , (||)
  , not
  , otherwise
  )

import Data.Coerce as Exports
  ( coerce
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
  , traverse_
  )

import Data.Function as Exports
  ( ($)
  , (&)
  , (.)
  , const
  , flip
  )

import Data.Functor as Exports
  ( Functor
  , ($>)
  , (<$)
  , (<$>)
  , (<&>)
  , fmap
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

import Data.String as Exports
  ( String
  )

import Data.Text as Exports
  ( Text
  )

import Data.Traversable as Exports
  ( traverse
  )

import Data.Tuple as Exports
  ( uncurry
  )

import GHC.Enum as Exports
  ( Bounded
  , Enum
  , maxBound
  , minBound
  )

import GHC.Generics as Exports
  ( Generic
  )

import GHC.Num as Exports
  ( Num
  , (*)
  , (+)
  )

import Numeric.Natural as Exports
  ( Natural
  )

import System.IO as Exports
  ( IO
  )

import Text.Show as Exports
  ( Show
  , show
  )

identity :: a -> a
identity value = value
