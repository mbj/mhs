module MIO.Core
  ( MIO(..)
  , liftMIO
  , mapMIO
  , runMIO
  )
where

import           Control.Applicative            ( liftA2 )
import           Control.Monad.Catch            ( MonadCatch, MonadThrow )
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
newtype MIO env a = MIO { unMIO :: ReaderT env IO a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader env, MonadThrow, MonadCatch)

instance Semigroup a => Semigroup (MIO env a) where
  (<>) = liftA2 (<>)

instance Monoid a => Monoid (MIO env a) where
  mempty  = pure mempty

-- | Using the environment run in IO the action that requires that environment.
--
-- @since 0.0.1.0
runMIO :: MonadIO m => env -> MIO env a -> m a
runMIO env (MIO (ReaderT f)) = liftIO (f env)

-- | Abstract `MIO` to an arbitrary `MonadReader` instance, which can handle IO.
--
-- @since 0.0.1.0
liftMIO :: (MonadIO m, MonadReader env m) => MIO env a -> m a
liftMIO action = do
  env <- ask
  runMIO env action

-- | Lift one MIO env to another.
--
-- @since 0.1.13.0
mapMIO :: (outer -> inner) -> MIO inner a -> MIO outer a
mapMIO f m = do
  outer <- ask
  runMIO (f outer) m

instance MonadUnliftIO (MIO env) where
  withRunInIO inner = MIO $ withRunInIO $ \run -> inner (run . (.unMIO))
  {-# INLINE withRunInIO #-}

instance PrimMonad (MIO env) where
  type PrimState (MIO env) = PrimState IO
  primitive = MIO . ReaderT . const . primitive
