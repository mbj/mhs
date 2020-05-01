module OpenApi
  ( module OpenApi.Description
  , module OpenApi.TaggedText
  , module OpenApi.Types
  , loadSpecFileJSON
  , loadSpecFileYAML
  )
where

import OpenApi.Description
import OpenApi.Prelude
import OpenApi.TaggedText
import OpenApi.Types

import qualified Data.Aeson           as JSON
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Yaml            as YAML
import qualified System.Path          as Path

loadSpecFileJSON
  :: forall m .(MonadFail m, MonadIO m)
  => Path.AbsRelFile
  -> m Specification
loadSpecFileJSON = loadSpec <=< (liftIO . LBS.readFile . Path.toString)
  where
    loadSpec :: LBS.ByteString -> m Specification
    loadSpec
      = either (fail . ("Specification JSON decode failed: " <>)) pure
      . JSON.eitherDecode'

loadSpecFileYAML
  :: forall m .(MonadFail m, MonadIO m)
  => Path.AbsRelFile
  -> m Specification
loadSpecFileYAML = loadSpec <=< (liftIO . BS.readFile . Path.toString)
  where
    loadSpec :: BS.ByteString -> m Specification
    loadSpec
      = either (fail . ("Specification YAML decode failed: " <>). show) pure
      . YAML.decodeEither'
