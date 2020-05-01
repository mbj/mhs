module OpenApi
  ( module OpenApi.Description
  , module OpenApi.Types
  , loadSpecFile
  )
where

import Data.ByteString.Lazy (ByteString)
import OpenApi.Description
import OpenApi.Prelude
import OpenApi.Types

import qualified Data.Aeson           as JSON
import qualified Data.ByteString.Lazy as LBS
import qualified System.Path          as Path

loadSpecFile
  :: forall m .(MonadFail m, MonadIO m)
  => Path.AbsRelFile
  -> m Specification
loadSpecFile = loadSpec <=< (liftIO . LBS.readFile . Path.toString)
  where
    loadSpec :: ByteString -> m Specification
    loadSpec
      = either (fail . ("Specification JSON decode failed: " <>)) pure
      . JSON.eitherDecode'
