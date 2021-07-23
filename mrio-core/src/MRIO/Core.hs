-- | This module takes code from RIO.Prelude.RIO and reproduces it here
--  so that we can propose rio-core package to the rio team.
--
{-# OPTIONS -Wno-noncanonical-monoid-instances #-}
module MRIO.Core
  ( RIO(..)
  , liftRIO
  , mapRIO
  , runRIO
  )
where

import           Control.Applicative            ( liftA2 )
import           Control.Monad.Catch            ( MonadThrow )
import           Control.Monad.IO.Unlift        ( MonadIO(..)
                                                , MonadUnliftIO(..)
                                                )
import           Control.Monad.Primitive        ( PrimMonad(..) )
import           Control.Monad.Reader           ( MonadReader(..)
                                                , ReaderT(..)
                                                )
import           Prelude

-- | The Reader+IO monad. This is different from a 'ReaderT' because:
--
-- * It's not a transformer, it hardcodes IO for simpler usage and
-- error messages.
--
-- * Instances of typeclasses like 'MonadLogger' are implemented using
-- classes defined on the environment, instead of using an
-- underlying monad.
newtype RIO env a = RIO { unRIO :: ReaderT env IO a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader env, MonadThrow)

instance Semigroup a => Semigroup (RIO env a) where
  (<>) = liftA2 (<>)

instance Monoid a => Monoid (RIO env a) where
  mempty  = pure mempty
  mappend = liftA2 mappend

-- | Using the environment run in IO the action that requires that environment.
--
-- @since 0.0.1.0
runRIO :: MonadIO m => env -> RIO env a -> m a
runRIO env (RIO (ReaderT f)) = liftIO (f env)

-- | Abstract `RIO` to an arbitrary `MonadReader` instance, which can handle IO.
--
-- @since 0.0.1.0
liftRIO :: (MonadIO m, MonadReader env m) => RIO env a -> m a
liftRIO rio = do
  env <- ask
  runRIO env rio

-- | Lift one RIO env to another.
--
-- @since 0.1.13.0
mapRIO :: (outer -> inner) -> RIO inner a -> RIO outer a
mapRIO f m = do
  outer <- ask
  runRIO (f outer) m

instance MonadUnliftIO (RIO env) where
  withRunInIO inner = RIO $ withRunInIO $ \run -> inner (run . unRIO)
  {-# INLINE withRunInIO #-}

instance PrimMonad (RIO env) where
  type PrimState (RIO env) = PrimState IO
  primitive = RIO . ReaderT . const . primitive
