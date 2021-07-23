{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Conversions where

import           Control.Applicative
import           Control.Exception              ( Exception )
import           Control.Monad                  ( MonadPlus(..) )
import           Control.Monad.Catch            ( MonadThrow
                                                , throwM
                                                )
import           Control.Monad.Except           ( MonadError
                                                , throwError
                                                )
import           Data.Coerce                    ( Coercible
                                                , coerce
                                                )
import           Data.Int                       ( Int16
                                                , Int32
                                                , Int64
                                                , Int8
                                                )
import           Data.Text                      ( Text )
import           Data.Typeable                  ( Typeable )
import           Data.Word                      ( Word16
                                                , Word32
                                                , Word64
                                                , Word8
                                                )
import           Numeric.Natural                ( Natural )
import           Prelude                 hiding ( max
                                                , min
                                                )

import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as LBS
import qualified Data.Text                     as Text
import qualified Data.Text.Encoding            as Text
import qualified Data.Text.Lazy                as Text.Lazy
import qualified GHC.Show                      as Show
import qualified Language.Haskell.TH.Syntax    as TH

data BoundError a b = (Bounded b, Show a, Show b) => BoundError a

instance Show.Show (BoundError a b) where
  show (BoundError value) = boundError value (minBound @b) (maxBound @b)

instance (Typeable a, Typeable b) => Exception (BoundError a b)

data UserBoundError a b = UserBoundError a b b
  deriving stock (Typeable)

instance (Show a, Show b) => Show.Show (UserBoundError a b) where
  show (UserBoundError value min max) = boundError value min max

instance (Show a, Show b, Typeable a, Typeable b) => Exception (UserBoundError a b)

class Conversion b a where
  convert :: a -> b

  default convert :: (Coercible a b) => a -> b
  convert = coerce

instance (MonadError (UserBoundError Int Natural) m) => Conversion (m Natural) Int where
  convert = convertErrorFromIntegral

instance (MonadError (UserBoundError Int64 Natural) m) => Conversion (m Natural) Int64 where
  convert = convertErrorFromIntegral

instance (MonadError (UserBoundError Int32 Natural) m) => Conversion (m Natural) Int32 where
  convert = convertErrorFromIntegral

instance (MonadError (UserBoundError Int16 Natural) m) => Conversion (m Natural) Int16 where
  convert = convertErrorFromIntegral

instance (MonadError (UserBoundError Int8 Natural) m) => Conversion (m Natural) Int8 where
  convert = convertErrorFromIntegral

instance (MonadError (UserBoundError Word Natural) m) => Conversion (m Natural) Word where
  convert = convertErrorFromIntegral

instance (MonadError (UserBoundError Word64 Natural) m) => Conversion (m Natural) Word64 where
  convert = convertErrorFromIntegral

instance (MonadError (UserBoundError Word32 Natural) m) => Conversion (m Natural) Word32 where
  convert = convertErrorFromIntegral

instance (MonadError (UserBoundError Word16 Natural) m) => Conversion (m Natural) Word16 where
  convert = convertErrorFromIntegral

instance (MonadError (UserBoundError Integer Text) m) => Conversion (m Natural) Integer where
  convert value = maybe (throwError $ UserBoundError value "0" "Natural") pure
    $ checkedFromIntegral value

instance (MonadError (UserBoundError Natural Int) m) => Conversion (m Int) Natural where
  convert = convertErrorFromNatural

instance (MonadError (UserBoundError Natural Int16) m) => Conversion (m Int16) Natural where
  convert = convertErrorFromNatural

instance (MonadError (UserBoundError Natural Int32) m) => Conversion (m Int32) Natural where
  convert = convertErrorFromNatural

instance (MonadError (UserBoundError Natural Int64) m) => Conversion (m Int64) Natural where
  convert = convertErrorFromNatural

instance Conversion a a where
  convert = id

instance Conversion Integer Int where
  convert = fromIntegral

instance Conversion Integer Word32 where
  convert = fromIntegral

instance Conversion Integer Word16 where
  convert = fromIntegral

instance Conversion Integer Word8 where
  convert = fromIntegral

instance Conversion Natural Word32 where
  convert = fromIntegral

instance Conversion Natural Word16 where
  convert = fromIntegral

instance Conversion Natural Word8 where
  convert = fromIntegral

instance Conversion Integer Natural where
  convert = fromIntegral

instance (MonadError (BoundError Integer Int) m) => Conversion (m Int) Integer where
  convert = convertBoundedFromIntegral

instance (MonadError (BoundError Integer Word32) m) => Conversion (m Word32)  Integer where
  convert = convertBoundedFromIntegral

instance (MonadError (BoundError Integer Word16) m) => Conversion (m Word16) Integer where
  convert = convertBoundedFromIntegral

instance (MonadError (BoundError Integer Word8) m) => Conversion (m Word8) Integer where
  convert = convertBoundedFromIntegral

instance Conversion LBS.ByteString BS.ByteString where
  convert = LBS.fromStrict

instance Conversion BS.ByteString LBS.ByteString where
  convert = LBS.toStrict

instance Conversion BS.ByteString Text where
  convert = Text.encodeUtf8

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

convertErrorFromIntegral
  :: forall a m
   . (Integral a, Bounded a, MonadError (UserBoundError a Natural) m)
  => a
  -> m Natural
convertErrorFromIntegral value =
  maybe (throwError $ UserBoundError value 0 maxBound') pure
    $ checkedFromIntegral value
 where
  maxBound' :: Natural
  maxBound' = fromIntegral $ maxBound @a

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

convertUnsafe :: forall b a e . (Conversion (Either e b) a, Show e) => a -> b
convertUnsafe = either (error . show) id . convertEither @b @a @e

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
