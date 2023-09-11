{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Conversions where

import Control.Exception (Exception)
import Control.Monad (MonadPlus(..), when)
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.Except (MonadError, throwError)
import Data.Coerce (Coercible)
import Data.Int (Int, Int16, Int32, Int64, Int8)
import Data.Scientific (Scientific)
import Data.Typeable (Typeable)
import Data.Word (Word, Word16, Word32, Word64, Word8)
import GHC.Num (Integer)
import GHC.Real (Integral, fromIntegral)
import MPrelude

import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as LBS
import qualified Data.Scientific               as Scientific
import qualified Data.Text                     as Text
import qualified Data.Text.Lazy                as Text.Lazy
import qualified GHC.Err                       as Err
import qualified GHC.Show                      as Show
import qualified Language.Haskell.TH.Syntax    as TH

data BoundError a b = (Bounded b, Show a, Show b) => BoundError a

instance Show.Show (BoundError a b) where
  show (BoundError value) = boundError value (minBound @b) (maxBound @b)

instance (Typeable a, Typeable b) => Exception (BoundError a b)

instance Conversion Text (BoundError a b) where
  convert = convert . show

data UserBoundError a b = UserBoundError a b b
  deriving stock (Typeable)

instance (Show a, Show b) => Show.Show (UserBoundError a b) where
  show (UserBoundError value min max) = boundError value min max

instance (Show a, Show b, Typeable a, Typeable b) => Exception (UserBoundError a b)

instance (Show a, Show b) => Conversion Text (UserBoundError a b) where
  convert = convert . show

class Conversion b a where
  convert :: a -> b

  default convert :: (Coercible a b) => a -> b
  convert = coerce

instance (MonadError (UserBoundError Int Natural) m) => Conversion (m Natural) Int where
  convert = convertErrorBounded

instance (MonadError (UserBoundError Int8 Natural) m) => Conversion (m Natural) Int8 where
  convert = convertErrorBounded

instance (MonadError (UserBoundError Int16 Natural) m) => Conversion (m Natural) Int16 where
  convert = convertErrorBounded

instance (MonadError (UserBoundError Int32 Natural) m) => Conversion (m Natural) Int32 where
  convert = convertErrorBounded

instance (MonadError (UserBoundError Int64 Natural) m) => Conversion (m Natural) Int64 where
  convert = convertErrorBounded

instance (MonadError (UserBoundError Integer Text) m) => Conversion (m Natural) Integer where
  convert value = do
    when (value < 0) $ throwError userBoundError

    maybe (throwError userBoundError) pure $ checkedFromIntegral value
    where
      userBoundError = UserBoundError value "0" "Natural"

instance (MonadError (UserBoundError Natural Int) m) => Conversion (m Int) Natural where
  convert = convertErrorFromNatural

instance (MonadError (UserBoundError Natural Int8) m) => Conversion (m Int8) Natural where
  convert = convertErrorFromNatural

instance (MonadError (UserBoundError Natural Int16) m) => Conversion (m Int16) Natural where
  convert = convertErrorFromNatural

instance (MonadError (UserBoundError Natural Int32) m) => Conversion (m Int32) Natural where
  convert = convertErrorFromNatural

instance (MonadError (UserBoundError Natural Int64) m) => Conversion (m Int64) Natural where
  convert = convertErrorFromNatural

instance (MonadError (UserBoundError Natural Word) m) => Conversion (m Word) Natural where
  convert = convertErrorFromNatural

instance (MonadError (UserBoundError Natural Word8) m) => Conversion (m Word8) Natural where
  convert = convertErrorFromNatural

instance (MonadError (UserBoundError Natural Word16) m) => Conversion (m Word16) Natural where
  convert = convertErrorFromNatural

instance (MonadError (UserBoundError Natural Word32) m) => Conversion (m Word32) Natural where
  convert = convertErrorFromNatural

instance (MonadError (UserBoundError Natural Word64) m) => Conversion (m Word64) Natural where
  convert = convertErrorFromNatural

instance Conversion a a where
  convert = identity

instance Conversion Integer Int where
  convert = fromIntegral

instance Conversion Integer Int8 where
  convert = fromIntegral

instance Conversion Integer Int16 where
  convert = fromIntegral

instance Conversion Integer Int32 where
  convert = fromIntegral

instance Conversion Integer Int64 where
  convert = fromIntegral

instance Conversion Integer Natural where
  convert = fromIntegral

instance Conversion Integer Word where
  convert = fromIntegral

instance Conversion Integer Word8 where
  convert = fromIntegral

instance Conversion Integer Word16 where
  convert = fromIntegral

instance Conversion Integer Word32 where
  convert = fromIntegral

instance Conversion Integer Word64 where
  convert = fromIntegral

instance Conversion Int Word8 where
  convert = fromIntegral

instance Conversion Int Word16 where
  convert = fromIntegral

instance Conversion Int Word32 where
  convert = fromIntegral

instance (MonadError (BoundError Word64 Int) m) => Conversion (m Int) Word64 where
  convert value = do
    when (value > convertImpure (maxBound @Int))
      $ throwError error

    maybe (throwError error) pure $ checkedFromIntegral value
    where
      error :: BoundError Word64 Int
      error = BoundError value

instance Conversion Natural Word where
  convert = fromIntegral

instance Conversion Natural Word8 where
  convert = fromIntegral

instance Conversion Natural Word16 where
  convert = fromIntegral

instance Conversion Natural Word32 where
  convert = fromIntegral

instance Conversion Natural Word64 where
  convert = fromIntegral

instance Conversion Scientific Integer where
  convert = (`Scientific.scientific` 0)

instance (MonadError (BoundError Integer Int) m) => Conversion (m Int) Integer where
  convert = convertBoundedFromIntegral

instance (MonadError (BoundError Integer Int8) m) => Conversion (m Int8) Integer where
  convert = convertBoundedFromIntegral

instance (MonadError (BoundError Integer Int16) m) => Conversion (m Int16) Integer where
  convert = convertBoundedFromIntegral

instance (MonadError (BoundError Integer Int32) m) => Conversion (m Int32) Integer where
  convert = convertBoundedFromIntegral

instance (MonadError (BoundError Integer Int64) m) => Conversion (m Int64) Integer where
  convert = convertBoundedFromIntegral

instance (MonadError (BoundError Integer Word) m) => Conversion (m Word) Integer where
  convert = convertBoundedFromIntegral

instance (MonadError (BoundError Integer Word8) m) => Conversion (m Word8) Integer where
  convert = convertBoundedFromIntegral

instance (MonadError (BoundError Integer Word16) m) => Conversion (m Word16) Integer where
  convert = convertBoundedFromIntegral

instance (MonadError (BoundError Integer Word32) m) => Conversion (m Word32) Integer where
  convert = convertBoundedFromIntegral

instance (MonadError (BoundError Integer Word64) m) => Conversion (m Word64) Integer where
  convert = convertBoundedFromIntegral

instance (MonadError (BoundError Int Word8) m) => Conversion (m Word8) Int where
  convert = convertBoundedFromIntegral

instance (MonadError (BoundError Int Word16) m) => Conversion (m Word16) Int where
  convert = convertBoundedFromIntegral

instance (MonadError (BoundError Int Word32) m) => Conversion (m Word32) Int where
  convert = convertBoundedFromIntegral

instance (MonadError (BoundError Int Word64) m) => Conversion (m Word64) Int where
  convert value = do
    when (value < 0) $ throwError error

    maybe (throwError error) pure $ checkedFromIntegral value
      where
        error = BoundError value

instance Conversion LBS.ByteString BS.ByteString where
  convert = LBS.fromStrict

instance Conversion BS.ByteString LBS.ByteString where
  convert = LBS.toStrict

instance Conversion Text.Lazy.Text Text where
  convert = Text.Lazy.fromStrict

instance Conversion Text Text.Lazy.Text where
  convert = Text.Lazy.toStrict

instance Conversion Text String where
  convert = Text.pack

instance Conversion String Text where
  convert = Text.unpack

type ToText a = Conversion Text a

convertText :: forall a b . (Conversion Text a, Conversion b Text) => a -> b
convertText = convert @b . convert @Text

convertErrorFromNatural
  :: forall a m
   . (Integral a, Bounded a, MonadError (UserBoundError Natural a) m)
  => Natural
  -> m a
convertErrorFromNatural value =
  maybe (throwError $ UserBoundError value minBound maxBound) pure
    $ checkedFromIntegral value

convertErrorBounded
  :: forall a m
   . (Integral a, Bounded a, MonadError (UserBoundError a Natural) m)
  => a
  -> m Natural
convertErrorBounded value = do
  when (value < 0) $ throwError userBoundError

  maybe (throwError userBoundError) pure
    $ checkedFromIntegral value
 where
  maxBound' :: Natural
  maxBound' = fromIntegral $ maxBound @a

  userBoundError :: UserBoundError a Natural
  userBoundError = UserBoundError value 0 maxBound'

convertBoundedFromIntegral
  :: forall a b m
   . ( Integral a
     , Show a
     , Show b
     , Num b
     , Bounded b
     , Conversion a b
     , MonadError (BoundError a b) m
     )
  => a
  -> m b
convertBoundedFromIntegral value =
  if convert (minBound @b) <= value && value <= convert (maxBound @b)
    then pure $ fromIntegral value
    else throwError $ BoundError value

checkedFromIntegral
  :: forall a b m . (MonadPlus m, Integral a, Integral b) => a -> m b
checkedFromIntegral value = guard' (fromIntegral converted == value) converted
 where
  converted :: b
  converted = fromIntegral value

convertEither :: forall b a e . (Conversion (Either e b) a) => a -> Either e b
convertEither = convert

convertImpure :: forall b a e . (HasCallStack, Conversion (Either e b) a, Show e) => a -> b
convertImpure = either (Err.error . show) identity . convertEither @b @a @e

convertThrow
  :: forall b a e m
   . (Conversion (Either e b) a, Exception e, MonadThrow m)
  => a
  -> m b
convertThrow = either throwM pure . convertEither @b @a @e

convertFail
  :: forall b a e m
   . (Conversion (Either e b) a, Show e, MonadFail m)
  => a
  -> m b
convertFail = either (fail . show) pure . convertEither @b @a @e

convertMaybe :: forall b a e . (Conversion (Either e b) a) => a -> Maybe b
convertMaybe = either (const empty) pure . convertEither @b @a @e

convertVia :: forall c b a . (Conversion b c, Conversion c a) => a -> b
convertVia = convert @b @c . convert @c @a

boundError :: forall a b . (Show a, Show b) => a -> b -> b -> String
boundError value min max =
  "Value should be between "
    <> show min
    <> " and "
    <> show max
    <> " but was "
    <> show value

mkTH
  :: forall a b e
   . (TH.Lift b, Exception e, Conversion (Either e b) a)
  => a
  -> TH.Q (TH.TExp b)
mkTH input =
  TH.TExp <$> either (fail . show) TH.lift (convertThrow @b @a @e input)

toText :: ToText a => a -> Text
toText = convert

-- | 'guard'' b a returns a if b is True, otherwise becomes 'mzero'.
guard' :: MonadPlus m => Bool -> a -> m a
guard' b a = if b then pure a else mzero
{-# INLINE guard' #-}

eitherThrow :: forall m e a. (MonadThrow m, Exception e) => Either e a -> m a
eitherThrow = either throwM pure
