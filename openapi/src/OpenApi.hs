module OpenApi where

import OpenApi.OpenApi
import OpenApi.Prelude

import qualified Data.Aeson           as JSON
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Yaml            as YAML
import qualified System.Path          as Path

loadSpecFileJSON
  :: forall m .(MonadFail m, MonadIO m)
  => Path.AbsRelFile
  -> m OpenApi
loadSpecFileJSON = loadSpec <=< (liftIO . LBS.readFile . Path.toString)
  where
    loadSpec :: LBS.ByteString -> m OpenApi
    loadSpec
      = either (fail . ("OpenApi JSON decode failed: " <>)) pure
      . JSON.eitherDecode'

loadSpecFileYAML
  :: forall m .(MonadFail m, MonadIO m)
  => Path.AbsRelFile
  -> m OpenApi
loadSpecFileYAML = loadSpec <=< (liftIO . BS.readFile . Path.toString)
  where
    loadSpec :: BS.ByteString -> m OpenApi
    loadSpec
      = either (fail . ("OpenApi YAML decode failed: " <>). show) pure
      . YAML.decodeEither'
