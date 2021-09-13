module AWS.Lambda.Runtime.Environment
  ( Environment (..)
  , DecodeError (..)
  , get
  )
where

import AWS.Lambda.Runtime.Prelude
import Data.Aeson (FromJSON, ToJSON)
import Data.Word (Word16)

import qualified System.Envy as Envy

newtype DecodeError = DecodeError Text
  deriving anyclass Exception
  deriving stock Show

data Environment = Environment
  { functionName       :: Text
  , functionVersion    :: Text
  , functionMemorySize :: Word16
  , logGroupName       :: Text
  , logStreamName      :: Text
  }
  deriving anyclass (ToJSON, FromJSON)
  deriving stock (Show, Generic)

instance Envy.FromEnv Environment where
  fromEnv = Envy.gFromEnvCustom Envy.Option
    { dropPrefixCount = 0
    , customPrefix    = "AWS_LAMBDA"
    }

get :: MonadIO m => m (Either DecodeError Environment)
get =
  first (DecodeError . convert) <$> liftIO (Envy.decodeEnv @Environment)
