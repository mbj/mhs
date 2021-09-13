module AWS.Lambda.Runtime.Env
  ( LambdaEnv (..)
  , LambdaEnvDecodeError (..)
  , getLambdaEnv
  )
where

import AWS.Lambda.Runtime.Prelude
import Data.Aeson (FromJSON, ToJSON)
import Data.Word (Word16)

import qualified System.Envy as Envy

newtype LambdaEnvDecodeError = LambdaEnvDecodeError Text
  deriving anyclass Exception
  deriving stock (Show, Generic)

data LambdaEnv = LambdaEnv
  { functionName       :: Text
  , functionVersion    :: Text
  , functionMemorySize :: Word16
  , logGroupName       :: Text
  , logStreamName      :: Text
  }
  deriving anyclass (ToJSON, FromJSON)
  deriving stock (Show, Generic)

instance Envy.FromEnv LambdaEnv where
  fromEnv = Envy.gFromEnvCustom Envy.Option
    { dropPrefixCount = 0
    , customPrefix    = "AWS_LAMBDA"
    }

getLambdaEnv :: IO (Either LambdaEnvDecodeError LambdaEnv)
getLambdaEnv =
  first (LambdaEnvDecodeError . convert) <$> Envy.decodeEnv @LambdaEnv
